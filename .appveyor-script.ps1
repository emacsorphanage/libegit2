# For some reason, the return code of emacs isn't correctly communicated back to the script.
# Instead, we store the stdout/stderr and grep for "FAILED".
function Run-Test {
    param( $TestName )
    $command = "C:\emacs\bin\emacs --batch -L build -L test -l libegit2 -l test-helper -l $($TestName)-test -f ert-run-tests-batch-and-exit"
    $output = Invoke-Expression "$command 2>&1"
    Write-Host $output
    if ($output | select-string -Pattern "FAILED") {
        exit 1
    }
    if ($output | select-string -Pattern "Cannot open load file") {
        exit 1
    }
}

Run-Test -TestName "annotated-commit"
Run-Test -TestName "blame"
Run-Test -TestName "blob"
Run-Test -TestName "branch"
Run-Test -TestName "checkout"
Run-Test -TestName "cherrypick"
Run-Test -TestName "commit"
Run-Test -TestName "config"
Run-Test -TestName "describe"
Run-Test -TestName "diff"
Run-Test -TestName "graph"
Run-Test -TestName "ignore"
Run-Test -TestName "index"
Run-Test -TestName "merge"
Run-Test -TestName "message"
Run-Test -TestName "pathspec"
Run-Test -TestName "refcount"
Run-Test -TestName "reference"
Run-Test -TestName "reflog"
Run-Test -TestName "remote"
Run-Test -TestName "repository"
Run-Test -TestName "reset"
Run-Test -TestName "revert"
Run-Test -TestName "revwalk"
Run-Test -TestName "signature"
Run-Test -TestName "status"
Run-Test -TestName "submodule"
Run-Test -TestName "tag"
Run-Test -TestName "tree"
