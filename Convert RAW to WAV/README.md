# Convert audio .raw files to .wav files 

For Windows users. Based on [this solution](https://stackoverflow.com/questions/17667491/how-to-use-sox-in-windows).

1. Download [Sox](http://sox.sourceforge.net/)
2. Double click on the file you downloaded and install it.
3. Go to My Computer > Properties > Advanced System Settings > Environment Variables > System variables.
4. Select Path.
5. Click Edit > New :
6. Add this: C:\Program Files (x86)\sox-<CHECK YOUR VERSION NUMBER>\ As indicated in the example path, make sure to check what version of Sox you have installed by actually navigating to your Program Files (x86) folder and looking for a folder that starts with sox, for example sox-14-4-2.
7. Right click on ConvertRaw2Wav.cmd (on GitHub) > Save link as > put it in your folder which contains the RAW files. The download might be prevented by your browser. Say that you want to keep it anyway. Alternatively, you can copy/paste the content of ConvertRaw2Wav.cmd in the Notepad and save it as "ConvertRaw2Wav.cmd" to your folder which contains the RAW files.
8. Double click on ConvertRaw2Wav.cmd
9. Each RAW file should have a WAV copy appearing in the same folder

Note: It is designed for raw files recorded with a sample rate of 500000 Hz. So if you want to change it, right-click on the .cmd and choose "edit". Then change 500000 with your sample rate.