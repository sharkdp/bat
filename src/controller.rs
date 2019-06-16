use std::io::{self};
use std::path::Path;

use crate::app::{Config, PagingMode};
use crate::assets::HighlightingAssets;
use crate::errors::*;
use crate::inputfile::{InputFile};
use crate::output::OutputType;
use crate::printer::{InteractivePrinter, Printer, SimplePrinter};

pub struct Controller<'a> {
    config: &'a Config<'a>,
    assets: &'a HighlightingAssets,
}

impl<'b> Controller<'b> {
    pub fn new<'a>(config: &'a Config, assets: &'a HighlightingAssets) -> Controller<'a> {
        Controller { config, assets }
    }

    pub fn run(&self) -> Result<bool> {
        // Do not launch the pager if NONE of the input files exist
        let mut paging_mode = self.config.paging_mode;
        if self.config.paging_mode != PagingMode::Never {
            let call_pager = self.config.files.iter().any(|file| {
                if let InputFile::Ordinary(path) = file {
                    return Path::new(path).exists();
                } else {
                    return true;
                }
            });
            if !call_pager {
                paging_mode = PagingMode::Never;
            }
        }

        let mut output_type = OutputType::from_mode(paging_mode, self.config.pager)?;
        let writer = output_type.handle()?;
        let mut no_errors: bool = true;

        let stdin = io::stdin();

        for input_file in &self.config.files {
            match input_file.get_reader(&stdin) {
                Err(error) => {
                    handle_error(&error);
                    no_errors = false;
                }
                Ok(mut reader) => {
                    let result = if self.config.loop_through {
                        SimplePrinter::new().print_file(&mut reader, writer, *input_file)
                    } else {
                        InteractivePrinter::new(
                            &self.config,
                            &self.assets,
                            *input_file,
                            &mut reader,
                        ).print_file(&mut reader, writer, *input_file)
                    };

                    if let Err(error) = result {
                        handle_error(&error);
                        no_errors = false;
                    }
                }
            }
        }

        Ok(no_errors)
    }

}
