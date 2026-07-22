@echo off
setlocal enabledelayedexpansion

:: ==========================================
:: CONFIGURATION: Set your folder paths here
:: ==========================================
rem set "source_dir=D:\Github\lila-master\public\piece\merida"
rem set "target_dir=D:\Github\Grampus\src\Grampus\Images\Merida"
set "source_dir=D:\Github\lila-master\public\piece\horsey"
set "target_dir=D:\Github\Grampus\src\Grampus\Images\Horsey"
:: ==========================================

:: Check if ImageMagick is installed
where magick >nul 2>nul
if %errorlevel% neq 0 (
    echo Error: ImageMagick is not installed or not in your system PATH.
    pause
    exit /b
)

:: Validate Source Folder
if not exist "%source_dir%" (
    echo Error: Source folder "%source_dir%" does not exist.
    pause
    exit /b
)

:: Create Target Folder if it doesn't exist
if not exist "%target_dir%" (
    mkdir "%target_dir%"
)

echo Starting conversion...
echo -----------------------------------

:: Process files
for %%f in ("%source_dir%\*.svg") do (
    echo Converting: %%~nxf
    magick -background none "%%f" -resize 128x128 "%target_dir%\%%~nf.png"
)

echo -----------------------------------
echo Done! All SVGs converted to 128x128 PNGs.
pause
