# Convert DelphiCodeCoverage XML to Cobertura XML format
# For integration with Codecov.io
#
# USAGE:
#   .\Convert-ToCobertura.ps1 [-InputFile <path>] [-OutputFile <path>]
#
# EXAMPLE:
#   .\Convert-ToCobertura.ps1 -InputFile coverage_report\CodeCoverage_Summary.xml -OutputFile cobertura.xml

param(
    [string]$InputFile = "coverage_report\CodeCoverage_Summary.xml",
    [string]$OutputFile = "cobertura.xml",
    [string]$SourceRoot = "..\Source"
)

$ErrorActionPreference = "Stop"

# Use invariant culture for decimal formatting (dot separator)
[System.Threading.Thread]::CurrentThread.CurrentCulture = [System.Globalization.CultureInfo]::InvariantCulture

Write-Host "Converting DelphiCodeCoverage to Cobertura format..." -ForegroundColor Cyan

if (-not (Test-Path $InputFile)) {
    Write-Host "ERROR: Input file not found: $InputFile" -ForegroundColor Red
    exit 1
}

# Load source XML
[xml]$sourceXml = Get-Content $InputFile -Encoding UTF8

# Extract overall stats
$stats = $sourceXml.report.stats
$totalLines = [int]$stats.totallines.value
$coveredLines = [int]$stats.coveredlines.value
$lineRate = if ($totalLines -gt 0) { [math]::Round($coveredLines / $totalLines, 4) } else { 0 }

# Create Cobertura XML
$timestamp = [int][double]::Parse((Get-Date -UFormat %s))
$coberturaXml = [xml]@"
<?xml version="1.0" encoding="UTF-8"?>
<!DOCTYPE coverage SYSTEM "http://cobertura.sourceforge.net/xml/coverage-04.dtd">
<coverage line-rate="$lineRate" branch-rate="0" lines-covered="$coveredLines" lines-valid="$totalLines" branches-covered="0" branches-valid="0" complexity="0" version="1.0" timestamp="$timestamp">
    <sources>
        <source>$SourceRoot</source>
    </sources>
    <packages>
    </packages>
</coverage>
"@

$packagesNode = $coberturaXml.SelectSingleNode("//packages")

# Process each package (source file) from DelphiCodeCoverage
$allData = $sourceXml.report.data.all
foreach ($package in $allData.package) {
    $packageName = $package.name

    # Parse package coverage
    $packageCoverage = $package.coverage | Where-Object { $_.type -like "line*" }
    $packageLineRate = 0
    if ($packageCoverage) {
        if ($packageCoverage.value -match "(\d+)%\s*\((\d+)/(\d+)\)") {
            $covered = [int]$matches[2]
            $total = [int]$matches[3]
            $packageLineRate = if ($total -gt 0) { [math]::Round($covered / $total, 4) } else { 0 }
        }
    }

    # Create package element
    $packageElement = $coberturaXml.CreateElement("package")
    $packageElement.SetAttribute("name", $packageName)
    $packageElement.SetAttribute("line-rate", $packageLineRate.ToString())
    $packageElement.SetAttribute("branch-rate", "0")
    $packageElement.SetAttribute("complexity", "0")

    $classesElement = $coberturaXml.CreateElement("classes")

    # Process source files in package
    foreach ($srcfile in $package.srcfile) {
        $filename = $srcfile.name

        # Process classes in source file
        foreach ($class in $srcfile.class) {
            $className = $class.name

            # Parse class coverage
            $classCoverage = $class.coverage | Where-Object { $_.type -like "line*" }
            $classLineRate = 0
            $classCovered = 0
            $classTotal = 0
            if ($classCoverage) {
                if ($classCoverage.value -match "(\d+)%\s*\((\d+)/(\d+)\)") {
                    $classCovered = [int]$matches[2]
                    $classTotal = [int]$matches[3]
                    $classLineRate = if ($classTotal -gt 0) { [math]::Round($classCovered / $classTotal, 4) } else { 0 }
                }
            }

            # Create class element
            $classElement = $coberturaXml.CreateElement("class")
            $classElement.SetAttribute("name", $className)
            $classElement.SetAttribute("filename", $filename)
            $classElement.SetAttribute("line-rate", $classLineRate.ToString())
            $classElement.SetAttribute("branch-rate", "0")
            $classElement.SetAttribute("complexity", "0")

            # Create methods element
            $methodsElement = $coberturaXml.CreateElement("methods")

            foreach ($method in $class.method) {
                $methodName = $method.name

                # Parse method coverage
                $methodCoverage = $method.coverage | Where-Object { $_.type -like "line*" }
                $methodLineRate = 0
                $methodCovered = 0
                $methodTotal = 0
                if ($methodCoverage) {
                    if ($methodCoverage.value -match "(\d+)%\s*\((\d+)/(\d+)\)") {
                        $methodCovered = [int]$matches[2]
                        $methodTotal = [int]$matches[3]
                        $methodLineRate = if ($methodTotal -gt 0) { [math]::Round($methodCovered / $methodTotal, 4) } else { 0 }
                    }
                }

                # Create method element
                $methodElement = $coberturaXml.CreateElement("method")
                $methodElement.SetAttribute("name", $methodName)
                $methodElement.SetAttribute("signature", "()")
                $methodElement.SetAttribute("line-rate", $methodLineRate.ToString())
                $methodElement.SetAttribute("branch-rate", "0")
                $methodElement.SetAttribute("complexity", "0")

                # Add empty lines element (required by Cobertura schema)
                $linesElement = $coberturaXml.CreateElement("lines")
                $methodElement.AppendChild($linesElement) | Out-Null

                $methodsElement.AppendChild($methodElement) | Out-Null
            }

            $classElement.AppendChild($methodsElement) | Out-Null

            # Add class-level lines element
            $classLinesElement = $coberturaXml.CreateElement("lines")
            $classElement.AppendChild($classLinesElement) | Out-Null

            $classesElement.AppendChild($classElement) | Out-Null
        }
    }

    $packageElement.AppendChild($classesElement) | Out-Null
    $packagesNode.AppendChild($packageElement) | Out-Null
}

# Save output
$coberturaXml.Save($OutputFile)

Write-Host "Converted successfully!" -ForegroundColor Green
Write-Host "  Input:  $InputFile" -ForegroundColor Gray
Write-Host "  Output: $OutputFile" -ForegroundColor Gray
Write-Host "  Line coverage: $([math]::Round($lineRate * 100, 2))% ($coveredLines/$totalLines)" -ForegroundColor Gray
