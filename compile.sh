tmpx -l mdbasic.lst -i mdbasic.asm -o mdbasic.prg
if [ $? -eq 0 ]; then 
  cp mdbasic.prg ~/c64/bowren/mdbasic.prg
  x64sc -silent -autostartwithcolon -keybuf "sys64738\n" ~/c64/bowren/mdbasic.prg &
fi
