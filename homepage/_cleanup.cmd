attrib ~*.php -h /s
del ~*.php /s

attrib *.bak -h /s
del *.bak /s

del .\site\files\cache\*.* /Q

del .\site\files\temp\*.* /Q

echo. > .\site\files\cache\void

pause
