#requires -PSEdition Core

function Get-Brittany {
	$dir = 'temp-brittany'
	New-Item -ItemType directory -Path $dir
	git clone --depth 1 --branch 0.13.1.0 https://github.com/lspitzner/brittany $dir
	Set-Location $dir
	stack build --stack-yaml stack-8.8.4.yaml --copy-compiler-tool --copy-bins
	Set-Location ..
	Remove-Item -Recurse -Force -Path $dir
}

function Invoke-All-Days {
	stack run
}

Switch ($args[0]) {
	"setup" {
		Get-Brittany
		stack build --copy-compiler-tools ghcid
		break
	}

	"setup:brittany" {
		Get-Brittany
		break
	}

	"ghcid" {
		stack exec ghcid
		break
	}

	"run" {
		Invoke-All-Days
		break
	}
	"run:all" {
		Invoke-All-Days
		break
	}

	"" {
		'No argument passed'
		break
	}

	default {
		$a = $args[0]
		"$a is not a recognized command"
		break
	}
}
