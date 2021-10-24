# MDBASIC
MDBASIC is an extension to the Commodore 64 BASIC interpreter.<br>
MDBASIC version 2021.10.3<br>
<br>
Download the MS Word document mdbasic.doc for details about each command.<br>
<br>
Download the VICE emulator for the Commodore 64:<br>
http://vice-emu.sourceforge.net/<br>
<br>
Download KickAssembler to compile source:<br>
https://www.c64-wiki.com/wiki/KickAssembler<br>
<br>
Add these options to your KickAss.cfg file to automatically launch & reset after compile:<br>
<br>
-showmem<br>
-symbolfile<br>
-execute "{ViceDir}\bin\x64sc.exe +autostartwithcolon -chdir {startDir} -keybuf sys64738\n"<br>
<br>
Compile source using KickAssembler in a command shell:<br>
java -jar kickass.jar mdbasic.asm<br>
<br>
<br>
<br>
<div>Legal:</div>
<div>
This program is free software; you can redistribute it and/or
modify it under the terms of the Apache Public License 2.0 as
published by the Apache Software Foundation; either version 2
of the License, or (at your option) any later version.<br>
<br>
This program is distributed in the hope that it will be useful,
but WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
Apache General Public License 2.0 for more details.
</div>
