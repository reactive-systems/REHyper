use std::env;
use std::error::Error;
use std::fs::File;
use std::io::{self, BufRead, BufReader, BufWriter, Write};
use std::path::{Path, PathBuf};
use std::process;
use std::time::Instant;

use clap::{App, AppSettings, Arg, crate_version};
use hyperltl::*;
use log::{debug, error, info};

mod enforcer;
use enforcer::*;

mod tseitin;

#[derive(Debug, Copy, Clone)]
enum Mode {
    Parallel,
    ParallelReact,
    ParallelN(usize),
    ParallelNReact(usize),
    Sequential,
    SequentialReact
}

fn main() {
    let start = Instant::now();
    env_logger::Builder
        ::from_env(
            env_logger::Env::new()
                .default_filter_or("info")
        )
        .format(move |buf, record| {
            let diff = (Instant::now() - start).as_micros();
            writeln!(buf, "[{:>6}.{:0>6}] {:>5}: {}",
                diff / 1000000, diff % 1000000,
                record.level(), record.args())
        })
        .init();

    match run() {
        Ok(ret) =>
            process::exit(ret),
        Err(err) => {
            error!("{}", err);
            process::exit(1);
        }
    }
}

fn run() -> Result<i32, Box<dyn Error>> {
    let matches = App::new("rehyper")
        .version(crate_version!())
        .setting(AppSettings::UnifiedHelpMessage)
        .arg(Arg::with_name("formula")
            .short("f")
            .long("formula")
            .value_name("FORMULA")
            .takes_value(true)
            .required(true)
            .display_order(1)
            .help("Specifies the HyperLTL formula to be enforced")
        )
        .arg(Arg::with_name("inputs")
            .short("i")
            .long("input")
            .value_name("APS")
            .display_order(2)
            .help("Specify input propositions as a comma seperated list of atomic propositions")
        )
        .arg(Arg::with_name("parallel")
            .short("P")
            .long("parallel")
            .display_order(3)
            .help("Parallel monitoring (default if at least one FILE is given)")
        )
        .arg(Arg::with_name("parallel-n")
            .short("X")
            .long("parallel-n")
            .display_order(3)
            .value_name("N")
            .help("Parallel monitoring where each FILE contains N traces")
        )
        .arg(Arg::with_name("sequential")
            .short("S")
            .long("sequential")
            .display_order(3)
            .help("Sequential monitoring (default if no FILES are given)")
        )
        .arg(Arg::with_name("output-files")
            .short("O")
            .long("output-files")
            .multiple(true)
            .value_name("FILES")
            .display_order(4)
            .help("Output files")
        )
        .arg(Arg::with_name("arena-cache")
            .long("arena-cache")
            .value_name("FILE")
            .display_order(5)
            .help("Cache file for arenas")
        )
        .arg(Arg::with_name("files")
            .index(1)
            .multiple(true)
            .value_name("FILES")
            .help("Input files")
        )
        .get_matches();

    let f = matches.value_of("formula").unwrap();
    let formula = parse_hyperltl(&f)?;
    debug!("Formula: {}", formula);

    let inputs =
        match matches.value_of("inputs") {
            Some(val) =>
                parse_event(val)
                    .map_err(|_| "invalid input specification")?
                    .into_iter()
                    .map(From::from)
                    .collect(),
            None =>
                vec![]
        };
    debug!("Inputs: {:?}", inputs);

    let paths = matches.values_of("files")
        .map_or_else(|| vec![], |it| it.map(From::from).collect());
    debug!("Files: {:?}", paths);

    let output_paths: Option<Vec<String>> = matches.values_of("output-files")
        .map(|it| it.map(From::from).collect());
    debug!("Output-files: {:?}", output_paths);

    let arena_cache = matches.value_of("arena-cache")
        .map(|s| Path::new(s));
    debug!("Arena-Cache: {:?}", arena_cache);

    let mode =
        if matches.is_present("parallel") {
            if inputs.is_empty() {
                Mode::Parallel
            } else {
                Mode::ParallelReact
            }
        } else if let Some(n) = matches.value_of("parallel-n") {
            if let Ok(n) = n.parse::<usize>() {
                if n == 0 {
                    Err("number of traces must be at least 1".to_string())?;
                }
                if inputs.is_empty() {
                    Mode::ParallelN(n)
                } else {
                    Mode::ParallelNReact(n)
                }
            } else {
                return Err(format!("invalid number: {}", n).into());
            }
        } else if matches.is_present("sequential") {
            if inputs.is_empty() {
                Mode::Sequential
            } else {
                Mode::SequentialReact
            }
        } else if paths.is_empty() {
            if inputs.is_empty() {
                Mode::Sequential
            } else {
                Mode::SequentialReact
            }
        } else {
            if inputs.is_empty() {
                Mode::Parallel
            } else {
                Mode::ParallelReact
            }
        };
    debug!("Mode: {:?}", mode);
    debug!("");

    if let Some(output_paths) = &output_paths {
        match mode {
            Mode::ParallelN(n) | Mode::ParallelNReact(n) =>
                if output_paths.len() != n {
                    Err(format!("given {} traces, but {} output files", n, output_paths.len()))?;
                },
            _ =>
                if output_paths.len() != paths.len() {
                    Err(format!("given {} input files, but {} output files", paths.len(), output_paths.len()))?;
                }
        }
    }

    let mut files = Vec::new();
    for next in &paths {
        let f = File::open(next)
            .map_err(|err| format!("cannot open input file {}: {}", next, err))?;
        files.push(BufReader::new(f));
    }

    let files_out = match &output_paths {
        Some(output_paths) => {
            let mut files_out = Vec::new();
            for next in output_paths {
                let f = File::create(next)
                    .map_err(|err| format!("cannot open output file {}: {}", next, err))?;
                files_out.push(BufWriter::new(f));
            }
            Some(files_out)
        },
        None =>
            None
    };

    match mode {
        Mode::Parallel => {
            info!("initializing enforcer (parallel non-reactive)");
            let enforcer = NonReactiveParEnforcer::new(&formula, files.len())?;
            info!("start monitoring");
            run_parallel(paths, files, files_out, enforcer)?;
        },
        Mode::ParallelReact => {
            info!("initializing enforcer (parallel reactive)");
            let enforcer = ReactiveParEnforcer::new_using_cache(&formula, &inputs, files.len(), arena_cache)?;
            info!("start monitoring");
            run_parallel(paths, files, files_out, enforcer)?;
        },
        Mode::ParallelN(n) => {
            info!("initializing enforcer (parallel non-reactive)");
            let enforcer = NonReactiveParEnforcer::new(&formula, n)?;
            info!("start monitoring");
            if files.is_empty() {
                let stdin = std::io::stdin();
                let stdin = stdin.lock();
                run_parallel_n(n, vec!["stdin".to_string()], vec![stdin], files_out, enforcer)?;
            } else {
                run_parallel_n(n, paths, files, files_out, enforcer)?;
            }
        },
        Mode::ParallelNReact(n) => {
            info!("initializing enforcer (parallel reactive)");
            let enforcer = ReactiveParEnforcer::new_using_cache(&formula, &inputs, n, arena_cache)?;
            info!("start monitoring");
            if files.is_empty() {
                let stdin = std::io::stdin();
                let stdin = stdin.lock();
                run_parallel_n(n, vec!["stdin".to_string()], vec![stdin], files_out, enforcer)?;
            } else {
                run_parallel_n(n, paths, files, files_out, enforcer)?;
            }
        },
        Mode::Sequential => {
            info!("initializing enforcer (sequential non-reactive)");
            let enforcer = NonReactiveSeqEnforcer::new(&formula)?;
            info!("start monitoring");
            if files.is_empty() {
                let stdin = std::io::stdin();
                let stdin = stdin.lock();
                run_sequential(vec!["stdin".to_string()], vec![stdin], files_out, enforcer)?;
            } else {
                run_sequential(paths, files, files_out, enforcer)?;
            }
        },
        Mode::SequentialReact => {
            info!("initializing enforcer (sequential reactive)");
            let enforcer = ReactiveSeqEnforcer::new(&formula, &inputs)?;
            info!("start monitoring");
            if files.is_empty() {
                let stdin = std::io::stdin();
                let stdin = stdin.lock();
                run_sequential(vec!["stdin".to_string()], vec![stdin], files_out, enforcer)?;
            } else {
                run_sequential(paths, files, files_out, enforcer)?;
            }
        },
    };

    info!("rehyper exiting normally");
    Ok(0)
}

fn run_parallel<R: BufRead, W: Write, E: ParEnforcer>(
    paths: Vec<String>,
    mut files: Vec<R>,
    mut files_out: Option<Vec<W>>,
    mut enforcer: E
) -> Result<(), Box<dyn Error>> {
    let mut lines = vec![String::new(); files.len()];
    loop {
        let mut remaining = false;
        for (i, next) in files.iter_mut().enumerate() {
            lines[i].clear();
            let n = next.read_line(&mut lines[i])
                .map_err(|err| format!("{}: {}", paths[i], err))?;
            if n == 0 {
                debug!("{} EOF", paths[i]);
            } else {
                remaining = true;
                debug!("{}: {}", paths[i], lines[i].trim());
            }
        }

        if !remaining {
            break;
        }

        enforce_parallel(&mut files_out, &mut enforcer, &lines)?;
    };

    while let Some(evts) = enforcer.finish() {
        print_parallel(&mut files_out, &evts)?;
    }

    Ok(())
}

fn run_parallel_n<R: BufRead, W: Write, E: ParEnforcer>(
    n: usize,
    paths: Vec<String>,
    mut files: Vec<R>,
    mut files_out: Option<Vec<W>>,
    mut enforcer: E
) -> Result<(), Box<dyn Error>> {
    let mut line = String::new();
    for (i, next) in files.iter_mut().enumerate() {
        loop {
            line.clear();
            let rc = next.read_line(&mut line)
                .map_err(|err| format!("{}: {}", paths[i], err))?;
            if rc == 0 {
                debug!("{} EOF", paths[i]);
                break;
            }

            let line = line.trim();
            debug!("{}: {}", paths[i], line);

            let lines: Vec<_> = line.split('|').collect();
            if lines.len() != n {
                Err(format!("line contains {} traces, expected {}", lines.len(), n))?;
            }

            enforce_parallel(&mut files_out, &mut enforcer, &lines)?;
        }
    }

    while let Some(evts) = enforcer.finish() {
        print_parallel(&mut files_out, &evts)?;
    }

    Ok(())
}

fn enforce_parallel<W: Write, E: ParEnforcer, S: std::ops::Deref<Target=str>>(
    files_out: &mut Option<Vec<W>>,
    enforcer: &mut E,
    lines: &[S]
) -> Result<(), Box<dyn Error>> {
    let mut evts = Vec::new();
    for next in lines {
        let aps = parse_event(next.trim())?;
        let mut evt = Event::new();
        for next in aps {
            evt.add(next.to_string());
        }
        evts.push(evt);
    }

    if let Some(evts2) = enforcer.enforce(&evts) {
        evts = evts2;
    }

    print_parallel(files_out, &evts)
}

fn print_parallel<W: Write>(
    files_out: &mut Option<Vec<W>>,
    evts: &[Event]
) -> Result<(), Box<dyn Error>> {
    if let Some(files_out) = files_out {
        for (i, next) in evts.iter().enumerate() {
            writeln!(files_out[i], "{}", next)?;
            files_out[i].flush()?;
        }
    } else {
        let stdout = io::stdout();
        let mut stdout = stdout.lock();
        for (i, next) in evts.iter().enumerate() {
            if i == 0 {
                write!(stdout, "{}", next)?;
            } else {
                write!(stdout, "|{}", next)?;
            }
        }
        writeln!(stdout, "")?;
        stdout.flush()?;
    }
    Ok(())
}

fn run_sequential<R: BufRead, W: Write, E: SeqEnforcer>(
    paths: Vec<String>,
    mut files: Vec<R>,
    mut files_out: Option<Vec<W>>,
    mut enforcer: E
) -> Result<(), Box<dyn Error>> {
    let mut n_traces = 0;
    for (i, next) in files.iter_mut().enumerate() {
        let mut file_out = match &mut files_out {
            Some(files_out) =>
                Some(&mut files_out[i]),
            None => {
                if i > 0 {
                    println!(";;");
                }
                None
            }
        };

        n_traces += 1;
        info!("trace #{} started", n_traces);
        enforcer.next();
        let mut line = String::new();
        loop {
            line.clear();
            let n = next.read_line(&mut line)
                .map_err(|err| format!("{}: {}", paths[i], err))?;
            let line = line.trim();
            if n == 0 {
                debug!("{} EOF", paths[i]);
                while let Some(evt) = enforcer.finish() {
                    print_sequential(&mut file_out, &evt)?;
                }
                break;
            } else if line == ";;" {
                debug!("{} EOT", paths[i]);
                while let Some(evt) = enforcer.finish() {
                    print_sequential(&mut file_out, &evt)?;
                }
                match &mut file_out {
                    Some(file_out) => {
                        writeln!(file_out, ";;")?;
                        file_out.flush()?;
                    },
                    None => {
                        println!(";;");
                    }
                }

                n_traces += 1;
                info!("trace #{} started", n_traces);
                enforcer.next();
            } else {
                debug!("{}: {}", paths[i], line.trim());
                enforce_sequential(&mut file_out, &mut enforcer, &line)?;
            }
        }
    };
    Ok(())
}

fn enforce_sequential<W: Write, E: SeqEnforcer>(
    file_out: &mut Option<&mut W>,
    enforcer: &mut E,
    line: &str
) -> Result<(), Box<dyn Error>> {
    let aps = parse_event(line.trim())?;
    let mut evt = Event::new();
    for next in aps {
        evt.add(next.to_string());
    }

    if let Some(evt2) = enforcer.enforce(&evt) {
        evt = evt2;
    }

    print_sequential(file_out, &evt)
}

fn print_sequential<W: Write>(
    file_out: &mut Option<&mut W>,
    evt: &Event
) -> Result<(), Box<dyn Error>> {
    if let Some(file_out) = file_out {
        writeln!(file_out, "{}", evt)?;
        file_out.flush()?;
    } else {
        let stdout = io::stdout();
        let mut stdout = stdout.lock();
        writeln!(stdout, "{}", evt)?;
        stdout.flush()?;
    }
    Ok(())
}

fn parse_event(evt: &'_ str) -> Result<Vec<&'_ str>, String> {
    if evt.is_empty() {
        return Ok(vec![]);
    }
    let mut res = Vec::new();
    let mut it = evt.char_indices();
    let mut curr = it.next();
    loop {
        match curr {
            Some((idx, delim @ '"')) | Some((idx, delim @ '\'')) => {
                curr = it.next();
                loop {
                    match curr {
                        Some((idx2, ch)) if ch == delim => {
                            res.push(&evt[(idx + 1)..idx2]);
                            curr = it.next();
                            break;
                        },
                        Some((_idx2, _ch)) =>
                            curr = it.next(),
                        None =>
                            return Err(format!("invalid event: {}", evt))
                    }
                }
                match curr {
                    Some((_idx2, ',')) =>
                        curr = it.next(),
                    None =>
                        return Ok(res),
                    _ =>
                        return Err(format!("invalid event: {}", evt))
                }
            },
            Some((idx, ch)) if ch.is_alphabetic() => {
                loop {
                    match curr {
                        Some((_idx2, ch)) if ch.is_alphanumeric() =>
                            curr = it.next(),
                        Some((idx2, ',')) => {
                            res.push(&evt[idx..idx2]);
                            curr = it.next();
                            break;
                        },
                        None => {
                            res.push(&evt[idx..]);
                            return Ok(res);
                        },
                        _ =>
                            return Err(format!("invalid event: {}", evt))
                    }
                }
            },
            _ =>
                return Err(format!("invalid event: {}", evt))
        }
    }
}

fn find_executable(name: &str) -> Option<PathBuf> {
    let mut path = std::env::current_exe().unwrap();
    assert!(path.pop());
    path.push(name);
    if path.is_file() {
        return Some(path);
    }

    let path = Path::new(".").join(name);
    if path.is_file() {
        return Some(path);
    }

    if let Some(paths) = env::var_os("PATH") {
        for next in env::split_paths(&paths) {
            let path = next.join(name);
            if path.is_file() {
                return Some(path);
            }
        }
    }

    None
}
