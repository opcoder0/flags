// extern crate flags;

use flags::{Flag, FlagSet};
use std::env;
use std::process;

fn main() {
    let bflag = Flag::new(
        Some("-b"),
        Some("--backup-path"),
        "path to the directory that can hold the backup files",
        true,
        Flag::kind::<String>(),
        Some(Box::new("/root/backup/10102022".to_string())),
    );
    let retry_flag = Flag::new(
        Some("-r"),
        Some("--retry"),
        "number of retry operations",
        false,
        Flag::kind::<i32>(),
        Some(Box::new(3i32)),
    );
    let mut flagset = FlagSet::new();
    flagset.add(&bflag);
    flagset.add(&retry_flag);
    let result = flagset.parse(&mut env::args());
    match result {
        Err(e) => {
            println!("{}", e); // prints the error
            println!("{}", flagset); // prints the usage
            process::exit(1);
        }
        Ok(()) => {}
    }
    let backup_path = bflag.borrow().get_value::<String>().ok().unwrap(); // extracts value for the mandatory flag
    let num_retries = retry_flag.borrow().get_value::<i32>().ok().unwrap(); // extracts value or default value for optional flag
    println!("backup path: {}", backup_path);
    println!("number of retries: {}", num_retries);
}
