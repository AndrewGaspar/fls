use std::vec::Vec;

pub struct Program {
    pub units: Vec<ProgramUnit>,
}

impl Program {
    pub fn new(units: Vec<ProgramUnit>) -> Program {
        Program { units: units }
    }
}

pub struct ProgramUnit {
    pub begin_name: Option<String>,
    pub end_name: Option<String>,
}

impl<'a> ProgramUnit {
    pub fn new(begin_name: Option<String>, end_name: Option<String>) -> ProgramUnit {
        ProgramUnit {
            begin_name: begin_name,
            end_name: end_name,
        }
    }
}
