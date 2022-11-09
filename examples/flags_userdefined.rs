extern crate flags;

use flags::{Flag, FlagSet};
use std::env;
use std::num::ParseIntError;
use std::process;
use std::str::FromStr;

#[derive(Debug, PartialEq)]
struct Point {
    x: i32,
    y: i32,
}

impl Clone for Point {
    fn clone(&self) -> Self {
        let x = self.x;
        let y = self.y;
        Self { x, y }
    }
}

impl ToString for Point {
    fn to_string(&self) -> String {
        format!("({},{})", self.x, self.y)
    }
}

impl FromStr for Point {
    type Err = ParseIntError;

    fn from_str(s: &str) -> Result<Self, Self::Err> {
        let (x, y) = s
            .strip_prefix('(')
            .and_then(|s| s.strip_suffix(')'))
            .and_then(|s| s.split_once(','))
            .unwrap();

        let x_fromstr = x.parse::<i32>()?;
        let y_fromstr = y.parse::<i32>()?;

        Ok(Point {
            x: x_fromstr,
            y: y_fromstr,
        })
    }
}

fn main() {
    let point_flag = Flag::new(
        Some("-p"),
        Some("--point"),
        "point on the 2d plane",
        true,
        Flag::kind::<Point>(),
        Some(Box::new(Point { x: 0, y: 0 })),
    );

    let mut flagset = FlagSet::new();
    flagset.add(&point_flag);
    let result = flagset.parse(&mut env::args());
    match result {
        Err(e) => {
            println!("{}", e);
            println!("{}", flagset);
            process::exit(1);
        }
        Ok(()) => {}
    }
    let coord = point_flag.borrow().get_value::<Point>().ok().unwrap();
    println!("co-ordinates: {:?}", coord);
}
