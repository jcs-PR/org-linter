@echo off
emacs -batch -Q -l %~dp0x-org-lint -- %*
