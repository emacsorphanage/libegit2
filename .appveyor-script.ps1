# For some reason, the return code of emacs isn't correctly communicated back to the script.
# Instead, we store the stdout/stderr and grep for "FAILED".
function Run-Test {
    param( $TestName )
    $command = "emacs --batch -L build -L test -l libegit2 -l test-helper -l $($TestName)-test -f ert-run-tests-batch-and-exit"
    $output = Invoke-Expression "$command 2>&1"
    Write-Host $output
    if ($output | select-string -Pattern "FAILED") {
        exit 1
    }
    if ($output | select-string -Pattern "Cannot open load file") {
        exit 1
    }
}

Run-Test -TestName "refcount"
Run-Test -TestName "reference"
Run-Test -TestName "repository"
