use cu::pre::*;

mod cmd_extract;
use cmd_extract::*;
use derive_more::AsRef;

#[derive(clap::Parser, AsRef)]
pub struct CmdMain {
    /// TOML config file
    #[clap(short = 'C', long)]
    pub config: String,

    #[clap(subcommand)]
    #[as_ref(cu::cli::Flags)]
    pub cmd: CmdSubcommand,
}

// impl AsRef<cu::cli::Flags> for CmdMain {
//     fn as_ref(&self) -> &cu::cli::Flags {
//         self.cmd.as_ref()
//     }
// }

#[derive(clap::Subcommand)]
pub enum CmdSubcommand {
    Extract(CmdExtract),
}

impl AsRef<cu::cli::Flags> for CmdSubcommand {
    fn as_ref(&self) -> &cu::cli::Flags {
        match self {
            Self::Extract(cmd) => cmd.as_ref(),
        }
    }
}

pub fn main(args: CmdMain) -> cu::Result<()> {
    let config = crate::config::load(args.config)?;

    match args.cmd {
        CmdSubcommand::Extract(cmd) => cmd.run(config),
    }
}
