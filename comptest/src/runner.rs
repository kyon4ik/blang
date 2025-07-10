use std::io::{self, Read};
use std::process::{Child, Command, ExitStatus, Stdio};

pub enum Poll<T> {
    Ready(T),
    Pending,
}

pub struct Runner<T: Task> {
    tasks: Vec<(T, usize)>,
    outputs: Vec<Option<<T as Task>::Output>>,
}

impl<T: Task> Default for Runner<T> {
    fn default() -> Self {
        Self {
            tasks: Vec::new(),
            outputs: Vec::new(),
        }
    }
}

impl<T: Task> Runner<T> {
    pub fn run(&mut self, task: impl IntoTask<IntoTask = T>) -> usize {
        let task_id = self.tasks.len();
        self.tasks.push((task.into_task(), task_id));
        task_id
    }

    pub fn wait(mut self) -> Vec<Option<<T as Task>::Output>> {
        self.outputs.resize_with(self.tasks.len(), || None);
        loop {
            if self.tasks.is_empty() {
                break;
            }

            self.tasks.retain_mut(|(task, id)| match task.poll() {
                Poll::Ready(output) => {
                    self.outputs[*id] = Some(output);
                    false
                }
                Poll::Pending => true,
            });
        }

        self.outputs
    }
}

pub trait Task: Sized {
    type Output;

    fn poll(&mut self) -> Poll<Self::Output>;

    fn map<U, F>(self, f: F) -> MapTask<Self, F>
    where
        F: Fn(Self::Output) -> U,
    {
        MapTask { task: self, map: f }
    }

    fn then<N>(self, next: N) -> ThenTask<Self, N>
    where
        N: IntoTask,
    {
        ThenTask {
            task: Either::First(self),
            next: Some(next),
        }
    }
}

pub trait IntoTask {
    type Output;
    type IntoTask: Task<Output = Self::Output>;

    fn into_task(self) -> Self::IntoTask;
}

impl IntoTask for Command {
    type Output = Result<Vec<u8>, ChildError>;

    type IntoTask = Child;

    fn into_task(mut self) -> Self::IntoTask {
        match self.stdout(Stdio::piped()).stderr(Stdio::piped()).spawn() {
            Ok(task) => task,
            Err(err) => panic!(
                "Into task for \"{}\" failed: {err}",
                self.get_program().display()
            ),
        }
    }
}

#[derive(Debug)]
pub enum ChildError {
    Fail { status: ExitStatus, stderr: Vec<u8> },
    Io(io::Error),
}

impl Task for Child {
    type Output = Result<Vec<u8>, ChildError>;

    fn poll(&mut self) -> Poll<Self::Output> {
        match self.try_wait() {
            Ok(Some(status)) => {
                let mut out = Vec::new();
                if status.success() {
                    self.stdout
                        .take()
                        .map(|mut stdout| stdout.read_to_end(&mut out));
                    Poll::Ready(Ok(out))
                } else {
                    self.stderr
                        .take()
                        .map(|mut stderr| stderr.read_to_end(&mut out));
                    Poll::Ready(Err(ChildError::Fail {
                        status,
                        stderr: out,
                    }))
                }
            }
            Err(err) => Poll::Ready(Err(ChildError::Io(err))),
            Ok(None) => Poll::Pending,
        }
    }
}

pub struct MapTask<T, F> {
    task: T,
    map: F,
}

impl<T, F, U> Task for MapTask<T, F>
where
    T: Task,
    F: Fn(T::Output) -> U,
{
    type Output = U;

    fn poll(&mut self) -> Poll<Self::Output> {
        match self.task.poll() {
            Poll::Ready(t) => Poll::Ready((self.map)(t)),
            Poll::Pending => Poll::Pending,
        }
    }
}

pub enum Either<T, U> {
    First(T),
    Second(U),
}

pub struct ThenTask<T, N: IntoTask> {
    task: Either<T, N::IntoTask>,
    next: Option<N>,
}

impl<T, N, X, Y, E> Task for ThenTask<T, N>
where
    T: Task<Output = Result<X, E>>,
    N: IntoTask<Output = Result<Y, E>>,
{
    type Output = Result<Y, E>;

    fn poll(&mut self) -> Poll<Self::Output> {
        match &mut self.task {
            Either::First(t1) => match t1.poll() {
                Poll::Ready(res) => match res {
                    Ok(_) => {
                        self.task = Either::Second(self.next.take().unwrap().into_task());
                        Poll::Pending
                    }
                    Err(e) => Poll::Ready(Err(e)),
                },
                Poll::Pending => Poll::Pending,
            },
            Either::Second(t2) => t2.poll(),
        }
    }
}

impl<T: Task> IntoTask for T {
    type Output = T::Output;

    type IntoTask = T;

    #[inline]
    fn into_task(self) -> Self::IntoTask {
        self
    }
}
