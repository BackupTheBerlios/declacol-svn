attrib ~*.php -h /s
del ~*.php /s

attrib *.bak -h /s
del *.bak /s

del .\site\files\cache\*.* /Q
pause
