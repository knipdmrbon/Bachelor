################################################################ Delete all lines from the files that are not needed ##########################################################
Get-ChildItem -Path *.names | %{Set-Content -Path header.txt -Value (get-content -Path $_.FullName | Select-String -Pattern '^(w|c)[a-zA-Z0-9_;([!$#]+' -AllMatches | % { $_.Matches } | % { $_.Value })} 
