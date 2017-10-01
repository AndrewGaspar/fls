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
    begin_name: Option<String>,
    end_name: Option<String>,
}

impl ProgramUnit {
    pub fn new(begin_name: Option<String>, end_name: Option<String>) -> ProgramUnit {
        ProgramUnit {
            begin_name: begin_name,
            end_name: end_name,
        }
    }

    pub fn name(self: &ProgramUnit) -> &str {
        self.begin_name.as_str()
    }
}
