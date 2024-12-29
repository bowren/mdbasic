tmpx -l mdbasic.lst -i mdbasic.asm -o mdbasic.prg
if [ $? -eq 0 ]; then 
  x64sc -silent -8 mdbasic.d64 -keybuf "sys64738\n" -autostart mdbasic.prg
fi
