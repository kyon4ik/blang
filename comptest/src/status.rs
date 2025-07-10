use std::fmt;
use std::str::FromStr;

#[derive(Clone, Copy, PartialEq, Eq)]
pub enum Status {
    OK,
    CF,
    LF,
    XX,
}

impl Status {
    pub const fn all() -> &'static [Self] {
        &[Self::OK, Self::CF, Self::LF, Self::XX]
    }

    pub const fn description(&self) -> &'static str {
        match self {
            Self::OK => "success",
            Self::CF => "compilation failed",
            Self::LF => "linking failed",
            Self::XX => "system error",
        }
    }

    pub fn style(&self) -> anstyle::Style {
        let ansi = match self {
            Status::OK => anstyle::AnsiColor::Green,
            Status::CF => anstyle::AnsiColor::Red,
            Status::LF => anstyle::AnsiColor::Magenta,
            Status::XX => anstyle::AnsiColor::Cyan,
        };
        anstyle::Style::new()
            .bold()
            .fg_color(Some(anstyle::Color::Ansi(ansi)))
    }

    pub fn styled(&self) -> String {
        format!("{}{}{}", self.style(), self, anstyle::Reset)
    }
}

impl FromStr for Status {
    type Err = ();

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        match s {
            "OK" => Ok(Status::OK),
            "CF" => Ok(Status::CF),
            "LF" => Ok(Status::LF),
            "XX" => Ok(Status::XX),
            _ => Err(()),
        }
    }
}

impl fmt::Display for Status {
    fn fmt(&self, f: &mut fmt::Formatter<'_>) -> fmt::Result {
        let s = match self {
            Status::OK => "OK",
            Status::CF => "CF",
            Status::LF => "LF",
            Status::XX => "XX",
        };
        f.write_str(s)
    }
}
