use bat::{
    self,
    assets::HighlightingAssets,
    inputfile::InputFile,
    line_range::{LineRange, LineRanges, RangeCheckResult},
    output::OutputType,
    printer::{InteractivePrinter, Printer},
    style::{OutputComponent, OutputComponents},
    Config,
};
use console::Term;
use std::{collections::HashSet, io};

fn main() -> bat::errors::Result<()> {
    let config = Config {
        term_width: Term::stdout().size().1 as usize,
        colored_output: true,
        true_color: true,
        theme: "1337".into(),
        line_ranges: LineRanges::from(vec![
            LineRange::from("5:7")?,
            LineRange::from("92:97")?,
            LineRange::from("15:17")?,
        ]),
        output_components: OutputComponents(with_full_decorations()),
        files: vec![InputFile::Ordinary("build.rs")],
        ..Default::default()
    };
    let assets = HighlightingAssets::new();

    let mut output_type = OutputType::from_mode(config.paging_mode, config.pager)?;
    let writer = output_type.handle()?;
    let stdin = io::stdin();

    for input_file in &config.files {
        let mut reader = input_file.get_reader(&stdin)?;
        let mut printer = InteractivePrinter::new(&config, &assets, *input_file, &mut reader);

        printer.print_header(writer, *input_file)?;

        if !reader.first_line.is_empty() {
            let mut line_buffer = Vec::new();
            let mut line_number = 1;
            let mut has_snip = true; // skip first line snip

            while reader.read_line(&mut line_buffer)? {
                match config.line_ranges.check(line_number) {
                    RangeCheckResult::OutsideRange => {
                        if !has_snip {
                            printer.print_snip(writer)?;
                            has_snip = true;
                        }
                    }
                    RangeCheckResult::InRange => {
                        printer.print_line(
                            /*out_of_range = */ false,
                            writer,
                            line_number,
                            &line_buffer,
                        )?;
                        has_snip = false;
                    }
                    RangeCheckResult::AfterLastRange => break,
                }

                line_number += 1;
                line_buffer.clear();
            }
        }

        printer.print_footer(writer)?;
    }

    Ok(())
}

fn with_full_decorations() -> HashSet<OutputComponent> {
    OutputComponent::Full
        .components(Default::default())
        .iter()
        .cloned()
        .collect()
}
