# brainwave cracktro v1

ZX-Spectrum crack releases intro by alx^bw.

## Youtube

[![Licence to kill crackro by alx youtube](https://img.youtube.com/vi/OU9Jh86ISqQ/0.jpg)](https://www.youtube.com/watch?v=OU9Jh86ISqQ "Licence to kill cracktro by alx on youtube")

[![Orbix the Terrorball crackro by alx youtube](https://img.youtube.com/vi/K8a_T3HDUjM/0.jpg)](https://www.youtube.com/watch?v=K8a_T3HDUjM "Orbix crackro by alx on youtube")

## TLDR

This sources is a Zilog Z80 Assembler in Alasm format. You should import and compile this source in
[Alasm](https://zxart.ee/rus/soft/tool/music/pro-tracker-alasm/qid:365628/) ZX-Spectrum assembler
[(I have used v5.08)](https://speccy.info/ALASM) to compile. Or convert this source manually for 
[SjASMPlus](http://speccy.info/SjASMPlus) which is crossplatform.

## Technical info

This intro use some software cheating to get non-standard ZX-Spectrum colour mode,
[Multicolour](http://speccy.info/%D0%9C%D1%83%D0%BB%D1%8C%D1%82%D0%B8%D0%BA%D0%BE%D0%BB%D0%BE%D1%80) with some border
effects synchronized for [Pentagon-128K 2+](https://speccy.info/Pentagon), a russian ZX-Spectrum clone. Actually when 
your code refresh colour attributes every line (at Pentagon it takes 224 CPU tact) by pushing the data using stack you
can get the multicolor effects like this one. You can also set a colour of the border when the ray finished screen line
output.

Here is a main templated sources and a version for Orbix. Probably other changes lost.

## Story

An oldschool and Commodore-64 style cracktro, but drawing the graphics on the screen border at ZX-Spectrum is not also
so easy. Both scenes spend a lot of people-hours (years?) counting their CPU tacts and optimize their routines. If you
don't know where the demoscene and into making comes from, please find the info first. 

In the end of 90s I was inspired to made not also a crack intro, just would love to fix firm bugs, performance, add
TR-DOS save/load, exam some history of game making and all these stuff. In those days cracking games on ZX-Spectrum
mostly went to the past, but still here was some revers-research. And yes, Russian users had their specific disk 
systems, so you need to adapt them from time to time. If you look on ZX-Spectrum archive you'll find a various TR-DOS
versions of the same game from different crack teams. The reason is only in making this better than another guys, as the
scene tradition.

Looking in the game code in debugger allows you to view a lot of details, some codestyle of the authors, find hidden
data, easter-eggs and brings you many happy hours. Sometimes you'll get the hidden messages inside the code tells you
why this game is so unfinished (because Martech freeze the development of 'War 1st' game, for example). And the loaders
of these games is a very special huge topic.

Another side of this process is signing your work, a cracktro. Here is a place where every team had their unique
technique and algorythms to show that your computer can much more than you imagine before. Border and sync effects, 
multicolours, maths, vector graphics all at your 8-bit computer!

## Related

- [Licence to kill crack intro on youtube](https://www.youtube.com/watch?v=OU9Jh86ISqQ)
- [Stainless Steel crack intro on youtube](https://www.youtube.com/watch?v=hqgBG23cQwE)
- [Orbix the Terrorball crack intro on youtube](https://www.youtube.com/watch?v=K8a_T3HDUjM)
- [Peter Pack Rat crack intro on youtube](https://www.youtube.com/watch?v=N3lLnC3z4uA)
- [Slightly Magic intro on youtube](https://www.youtube.com/watch?v=OZDoOETyyX4)
- [Canyon Warrior intro on youtube](https://www.youtube.com/watch?v=hWiqpjRIXos)
- [Orbix crack intro on pouet.net](https://www.pouet.net/prod.php?which=89862)
- [Licence To Kill Cractro on pouet.net](https://www.pouet.net/prod.php?which=89863)

## About

- [speccywiki](http://speccy.info/Brainwave)
- [wikipedia](https://ru.wikipedia.org/wiki/Brainwave_team)
- [brainwave at pouet.net](https://www.pouet.net/groups.php?which=715)
- [zxart.ee](https://zxart.ee/)
- [cracktro v2 sources](https://github.com/alexanderbazhenoff/brainwave-cracktro-v2)

