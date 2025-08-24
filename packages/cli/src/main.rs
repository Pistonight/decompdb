#[cu::cli]
fn main(args: decompdb_cli::cmds::CmdMain) -> cu::Result<()> {
    decompdb_cli::cmds::main(args)
}
