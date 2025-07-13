use std::fmt;
use std::io::{Read, Write};
use std::path::Path;
use std::process::{Child, Command, Stdio};

#[derive(Clone, Copy, PartialEq, Eq, Debug, Default)]
pub enum RunStatus {
    #[default]
    CompileError,
    RuntimeError,
    Success,
}

#[derive(Clone, Copy, PartialEq, Eq, Debug)]
pub enum TestStage {
    Compiling,
    Running,
}

struct ActiveStage {
    proc: Child,
    stage: TestStage,
}

pub struct Test<'path> {
    id: usize,
    path: &'path Path,
    active_stage: Option<ActiveStage>,
}

pub struct TestRunner<'p, S, M, R> {
    stager: S,
    out_map: M,
    active_tests: Vec<Test<'p>>,
    results: Vec<R>,
}

#[derive(PartialEq, Eq, Default)]
pub struct TestOutput {
    pub run_status: RunStatus,
    pub stdout: Vec<u8>,
    pub stderr: Vec<u8>,
}

pub trait TestStager {
    fn create(&self, stage: TestStage, test_path: &Path) -> Command;

    fn create_piped(&self, stage: TestStage, test_path: &Path) -> Command {
        let mut cmd = self.create(stage, test_path);
        cmd.stdout(Stdio::piped()).stderr(Stdio::piped());
        cmd
    }
}

impl<'p, S, M, R> TestRunner<'p, S, M, R>
where
    R: Default,
    M: Fn(&Path, TestOutput) -> R,
    S: TestStager,
{
    pub fn new(stager: S, out_map: M) -> Self {
        Self {
            stager,
            out_map,
            active_tests: vec![],
            results: vec![],
        }
    }

    pub fn run(&mut self, mut test: Test) -> R {
        loop {
            if let Some(output) = test.run(&self.stager, test.path) {
                return (self.out_map)(test.path, output);
            }
        }
    }

    pub fn submit(&mut self, mut test: Test<'p>) {
        test.id = self.active_tests.len();
        self.active_tests.push(test);
    }

    pub fn wait(mut self) -> Vec<R> {
        self.results
            .resize_with(self.active_tests.len(), Default::default);
        loop {
            if self.active_tests.is_empty() {
                break;
            }

            self.active_tests
                .retain_mut(|test| match test.run(&self.stager, test.path) {
                    Some(output) => {
                        self.results[test.id] = (self.out_map)(test.path, output);
                        false
                    }
                    None => true,
                });
        }

        self.results
    }
}

impl<'p> Test<'p> {
    pub fn new(path: &'p Path) -> Self {
        Self {
            id: 0,
            path,
            active_stage: None,
        }
    }

    fn run<S>(&mut self, stager: &S, path: &'p Path) -> Option<TestOutput>
    where
        S: TestStager,
    {
        match &mut self.active_stage {
            Some(active) => match active.proc.try_wait().unwrap() {
                Some(status) => {
                    if status.success() {
                        if let Some(next_stage) = active.stage.next() {
                            active.proc = stager.create_piped(next_stage, path).spawn().unwrap();
                            active.stage = next_stage;
                            None
                        } else {
                            Some(TestOutput::success(active))
                        }
                    } else {
                        Some(TestOutput::failure(active))
                    }
                }
                None => None,
            },
            None => {
                let stage = TestStage::first();
                let proc = stager.create_piped(stage, self.path).spawn().unwrap();
                self.active_stage = Some(ActiveStage::new(stage, proc));
                None
            }
        }
    }
}

fn split_once(bytes: &[u8], byte: u8) -> Option<(&[u8], &[u8])> {
    let pos = bytes.iter().position(|c| *c == byte)?;
    Some((&bytes[..pos], &bytes[pos + 1..]))
}

impl fmt::Display for TestOutput {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        writeln!(f, "Run status: {}", self.run_status)?;
        writeln!(f, "Stdout:\n{}", String::from_utf8_lossy(&self.stdout))?;
        writeln!(f, "Stderr:\n{}", String::from_utf8_lossy(&self.stderr))
    }
}

impl TestOutput {
    pub fn into_bytes(&self) -> Vec<u8> {
        let mut bytes = Vec::new();
        bytes.extend_from_slice(&self.run_status.into_bytes());
        bytes.push(b'\n');
        writeln!(bytes, "STDOUT [{}B]:", self.stdout.len()).unwrap();
        bytes.extend_from_slice(&self.stdout);
        bytes.push(b'\n');
        writeln!(bytes, "STDERR [{}B]:", self.stderr.len()).unwrap();
        bytes.extend_from_slice(&self.stderr);
        bytes.push(b'\n');
        bytes
    }

    pub fn from_bytes(bytes: &[u8]) -> Option<Self> {
        // run_status
        let (rs, bytes) = bytes.split_first_chunk()?;
        let run_status = RunStatus::from_bytes(*rs)?;
        let bytes = bytes.strip_prefix(b"\n")?;
        // STDOUT
        let (num, bytes) = split_once(bytes.strip_prefix(b"STDOUT [")?, b'B')?;
        let stdout_len = str::from_utf8(num).ok()?.parse::<usize>().ok()?;
        let bytes = bytes.strip_prefix(b"]:\n")?;
        let (stdout, bytes) = bytes.split_at_checked(stdout_len)?;
        let bytes = bytes.strip_prefix(b"\n")?;
        // STDERR
        let (num, bytes) = split_once(bytes.strip_prefix(b"STDERR [")?, b'B')?;
        let stderr_len = str::from_utf8(num).ok()?.parse::<usize>().ok()?;
        let bytes = bytes.strip_prefix(b"]:\n")?;
        let (stderr, bytes) = bytes.split_at_checked(stderr_len)?;
        let _bytes = bytes.strip_prefix(b"\n")?;
        Some(TestOutput {
            run_status,
            stdout: stdout.to_vec(),
            stderr: stderr.to_vec(),
        })
    }

    fn success(active: &mut ActiveStage) -> Self {
        let mut stdout = Vec::new();
        let mut stderr = Vec::new();
        let mut out = active.proc.stdout.take().unwrap();
        let mut err = active.proc.stderr.take().unwrap();

        out.read_to_end(&mut stdout).unwrap();
        err.read_to_end(&mut stderr).unwrap();

        Self {
            run_status: RunStatus::Success,
            stdout,
            stderr,
        }
    }

    fn failure(active: &mut ActiveStage) -> Self {
        let mut stdout = Vec::new();
        let mut stderr = Vec::new();
        let mut out = active.proc.stdout.take().unwrap();
        let mut err = active.proc.stderr.take().unwrap();

        out.read_to_end(&mut stdout).unwrap();
        err.read_to_end(&mut stderr).unwrap();

        let run_status = match active.stage {
            TestStage::Compiling => RunStatus::CompileError,
            TestStage::Running => RunStatus::RuntimeError,
        };
        Self {
            run_status,
            stdout,
            stderr,
        }
    }
}

impl ActiveStage {
    fn new(stage: TestStage, proc: Child) -> Self {
        Self { stage, proc }
    }
}

impl TestStage {
    pub fn next(&self) -> Option<TestStage> {
        match self {
            TestStage::Compiling => Some(Self::Running),
            TestStage::Running => None,
        }
    }

    pub fn first() -> Self {
        Self::Compiling
    }
}

impl RunStatus {
    pub fn from_bytes(bytes: [u8; 2]) -> Option<Self> {
        match &bytes {
            b"CE" => Some(Self::CompileError),
            b"RE" => Some(Self::RuntimeError),
            b"OK" => Some(Self::Success),
            _ => None,
        }
    }

    pub fn into_bytes(&self) -> [u8; 2] {
        *match self {
            RunStatus::CompileError => b"CE",
            RunStatus::RuntimeError => b"RE",
            RunStatus::Success => b"OK",
        }
    }
}

impl fmt::Display for RunStatus {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            Self::CompileError => "compilation error (CE)",
            Self::RuntimeError => "runtime error (RE)",
            Self::Success => "success (OK)",
        };
        f.write_str(str)
    }
}

impl fmt::Display for TestStage {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let str = match self {
            TestStage::Compiling => "compiling",
            TestStage::Running => "running",
        };
        f.write_str(str)
    }
}
