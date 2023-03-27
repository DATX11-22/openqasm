use std::path::Path;

use openqasm::openqasm::{
    parse_openqasm,
    semantic_analysis::{GateOperation, Operation},
};

fn main() {
    let example_dir = Path::new(file!()).parent().unwrap();
    let qasm_path = example_dir.join(Path::new("example.qasm"));
    println!("{:?}", qasm_path);
    let program = parse_openqasm(&qasm_path).unwrap();

    println!("Quantum registers:");
    for (name, reg_size) in program.qregs.iter() {
        println!("    {}[{}]", name, reg_size);
    }

    println!("Classical registers:");
    for (name, reg_size) in program.cregs.iter() {
        println!("    {}[{}]", name, reg_size);
    }

    println!("Gates:");
    for (name, gate) in program.gates.iter() {
        println!(
            "    {} | parameters: {}, arguments: {}",
            name, gate.num_arguments, gate.num_targets
        );

        for op in gate.operations.iter() {
            let gate_name = match op {
                GateOperation::U(_, _, _, _) => "U".to_string(),
                GateOperation::CX(_, _) => "CX".to_string(),
                GateOperation::Custom(gate_name, _, _) => gate_name.clone(),
            };
            println!("        {}", gate_name);
        }
    }

    println!("Operations:");
    for (cond, operation) in program.operations.iter() {
        print!("    ");

        if let Some(cond) = cond {
            print!("if({} == {}) ", cond.0, cond.1);
        }

        match operation {
            Operation::U(p1, p2, p3, qbit) => {
                println!("U({},{},{}) {}[{}]", p1, p2, p3, qbit.0, qbit.1)
            }
            Operation::CX(q1, q2) => println!("CX {}[{}], {}[{}]", q1.0, q1.1, q2.0, q2.1),
            Operation::Custom(name, ps, qs) => println!("{}({:?}) {:?}", name, ps, qs),
            Operation::Measure(q, c) => println!("measure {}[{}] -> {}[{}]", q.0, q.1, c.0, c.1),
            Operation::ResetQ(q) => println!("reset {}[{}]", q.0, q.1),
            Operation::ResetC(c) => println!("reset {}[{}]", c.0, c.1),
        }
    }
}
