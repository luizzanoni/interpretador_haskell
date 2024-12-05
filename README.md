# hs-interpreter-24-2

para instalar o GHCup, rodar o comando no PowerShell como adm

`Set-ExecutionPolicy Bypass -Scope Process -Force;[System.Net.ServicePointManager]::SecurityProtocol = [System.Net.ServicePointManager]::SecurityProtocol -bor 3072; try { & ([ScriptBlock]::Create((Invoke-WebRequest https://www.haskell.org/ghcup/sh/bootstrap-haskell.ps1 -UseBasicParsing))) -Interactive -DisableCurl } catch { Write-Error $_ }`

depois de instalar ele, deve colocar nas variávels de ambiente o ghc,
colocar de preferência nas variáveis do windows e NÃO do usuário

`C:\ghcup\ghc\9.4.8\bin`

use `ghc --make Main.hs -o lambda` para compilar o código no terminal do VSCode

e para rodar e ver o resultado, use por exemplo: `Echo "1+1" | .\lambda.exe` ou `Echo "if true then 1 else 0" | .\lambda.exe`