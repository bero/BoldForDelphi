$lines = Get-Content 'coverage_report\CodeCoverage_summary.html'
$joined = $lines -join ''
$rows = [regex]::Matches($joined, '>([A-Za-z_]+)</a><td>(\d+)<td>(\d+)<td>(\d+)<td[^>]*>(\d+)')
$results = @()
foreach ($r in $rows) {
    $name = $r.Groups[1].Value
    $covered = [int]$r.Groups[2].Value
    $uncov = [int]$r.Groups[3].Value
    $total = [int]$r.Groups[4].Value
    $pct = [int]$r.Groups[5].Value
    if ($uncov -gt 10 -and $pct -lt 80 -and $total -lt 300 -and $total -gt 20) {
        $results += [PSCustomObject]@{Unit=$name; Covered=$covered; Uncovered=$uncov; Total=$total; Coverage="$pct%"}
    }
}
$results | Sort-Object -Property Uncovered -Descending | Select-Object -First 30 | Format-Table -AutoSize
