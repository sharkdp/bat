"""Compare two release builds through a draining pager, checking output equality.

Usage: python3 tests/benchmarks/compare-output-writes.py BASELINE CANDIDATE
Build both with the same toolchain, release settings and default features.
This measures formatting and pipe overhead, not terminal rendering speed.
"""
import argparse
import os,subprocess,time,hashlib,statistics,json,pathlib,tempfile
parser = argparse.ArgumentParser(description=__doc__)
parser.add_argument("baseline", type=pathlib.Path)
parser.add_argument("candidate", type=pathlib.Path)
parser.add_argument("--runs", type=int, default=8)
args = parser.parse_args()
if args.runs < 1:
    parser.error("--runs must be positive")
run_count = args.runs
root=pathlib.Path(__file__).resolve().parents[2]
base=str(args.baseline.resolve(strict=True))
new=str(args.candidate.resolve(strict=True))
temporary = tempfile.TemporaryDirectory(prefix='bat-output-benchmark-')
data = pathlib.Path(temporary.name)
gitlog=data/'history.txt'
gitlog.write_bytes(b''.join((f'* \x1b[31m{i:08x}\x1b[0m -\x1b[33m (HEAD -> main)\x1b[0m A commit subject with more text for wrapping \x1b[32m(2 days ago)\x1b[0m \x1b[1;34m<Author Name>\x1b[0m\n').encode() for i in range(80000)))
seq=data/'sequence.txt';seq.write_text(''.join(f'{i}\n' for i in range(1,1000001)))
rust=data/'source.rs';rust.write_bytes((root/'src/printer.rs').read_bytes()*20)
report=[]
for name,path,extra in [('colored-history',gitlog,['--wrap=character','-l','txt']),('million-lines',seq,['--wrap=character','-l','txt']),('rust',rust,['--wrap=character','-l','rs']),('plain',gitlog,['--style=plain','--color=never'])]:
    args=['--no-config','--no-custom-assets','--color=always','--style=numbers','--terminal-width=80','--paging=always','--pager=cat']+extra+[str(path)]
    env={k:v for k,v in os.environ.items() if not k.startswith('BAT_') and k not in ['PAGER','LESS','NO_COLOR','COLORTERM','LESSOPEN','LESSCLOSE']};env['TERM']='xterm-256color'
    outputs=[subprocess.check_output([binary]+args,env=env) for binary in [base,new]]
    assert outputs[0]==outputs[1],name
    runs=[[],[]]
    for i in range(run_count):
        for index in ([0,1] if i%2==0 else [1,0]):
            start=time.perf_counter()
            subprocess.run([[base,new][index]]+args,stdout=subprocess.DEVNULL,check=True,env=env)
            runs[index].append(time.perf_counter()-start)
    row={'workload':name,'input_bytes':path.stat().st_size,'output_sha256':hashlib.sha256(outputs[0]).hexdigest(),'seconds':runs,'baseline_median':statistics.median(runs[0]),'new_median':statistics.median(runs[1])}
    row['speedup']=row['baseline_median']/row['new_median']
    report.append(row);print(json.dumps(row),flush=True)
temporary.cleanup()
