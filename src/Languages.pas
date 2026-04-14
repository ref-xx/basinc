unit Languages;

interface
  Uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math, ShellAPI, AxCtrls,
  FastIMG, ExtCtrls, StdCtrls, ComCtrls, FastDIB, FastDrawEx, FastFiles, FastSize, FastFX, GraphicEx,
  Utility, ROMUtils,AnimPreview;

  Procedure SetLanguage(Var lang: String);

 var

  ErrorsEN: Array[0..43, 1..4] of AnsiString =
  (('0', 'Ok', 'Successful completion, or jump to a line number bigger than any existing. This report does not change the line and statement jumped to by CONTINUE.', 'Any'),
   ('1', 'NEXT without FOR', 'The control variable does not exist (it has not been set up by a FOR statement), but there is an ordinary variable with the same name.', 'NEXT'),
   ('2', 'Variable not found', 'For a simple variable, this will happen if the variable is used'+' before it has been assigned to by a LET, READ or INPUT statement, loaded from disk, or set up in a FOR statement. For a subscripted variable, it will happen if the variable is used before it has been dimensioned in DIM statement, or loaded from disk.', 'Any'),
   ('3', 'Subscript Wrong', 'A subscript is beyond the dimension of the array, or there are the wrong number of subscripts. If the subscript is negative or bigger than 65535 then error B will result.', 'Subscripted variables, Substrings'),
   ('4', 'Out of memory', 'There is not enough room in memory for what you are trying to do. If the interpreter really seems to be stuck in this state then you may need to issue a CLEAR statement to make room by removing in-memory variables.', 'LET, INPUT, FOR, DIM, GO SUB, LOAD, MERGE. Sometimes during expression evaluation.'),
   ('5', 'Out of screen space', 'An INPUT statement has tried to generate more than 23 lines in the lower half of the screen. Also occurs with PRINT AT 22,xx.', 'INPUT, PRINT AT'),
   ('6', 'Number too big ', 'Calculations have yielded a number bigger than 10 raised to the power of 38.', 'Any arithmetic.'),
   ('7', 'RETURN without GO SUB', 'There has been one more RETURN than there were GO SUBs.', 'RETURN'),
   ('8', 'Fatal unknown error message', 'The interpreter has come across a statement or expression which it cannot handle. The program flow will be terminated.', 'Any expression or statement.'),
   ('9', 'STOP statement', 'After this, CONTINUE will not repeat the STOP, but will carry on with the statement after.', 'STOP'),
   ('A', 'Invalid Argument', 'The argument for a function is unsuitable (for some reason).', 'SQR, LN, ASC, ACS, USR (with String argument)'),
   ('B', 'Integer out of range', 'When an integer is required, the floating point argument is rounded to the nearest integer. If this is outside a suitable range, then this error results. For Array Access, also see Error 3.', 'RUN, RANDOMIZE, POKE, DIM, GO TO, GO SUB, LIST, LLIST, PAUSE, PLOT, CHR$, PEEK, USR (with numeric argument)'),
   ('C', 'Nonsense in BASIC', 'The text of the (string) argument does not form a valid expression. Also used when the argument for a function or command is outrageously wrong.', 'VAL, VAL$'),
   ('D', 'BREAK - CONTINUE repeats', 'BREAK was pressed during some peripheral operation. The behaviour of CONTINUE after this report is normal in that it repeats the statement. Compare with report L.', 'LOAD, SAVE, VERIFY, MERGE. Also when the computer asks "Scroll?" and you press N, BREAK or the space bar.'),
   ('E', 'Out of DATA', 'You have tried to READ past the end of the DATA list.', 'READ'),
   ('F', 'Invalid Filename', 'SAVE with a filename not descriptive of a file on your hard drive.', 'SAVE'),
   ('G', 'No room for line', 'There is not enough room left in program memory to accommodate the new line.', 'Entering a line into the program.'),
   ('H', 'STOP in INPUT', 'Some INPUT data started with the keyword "STOP". Unlike the case with report 9, after this report, CONTINUE will behave normally, by repeating the INPUT statement.', 'INPUT'),
   ('I', 'FOR without NEXT', 'There was a FOR loop to be executed no times (eg. FOR n=1 TO 0) and the corresponding NEXT statement could not be found.', 'FOR'),
   ('J', 'Invalid I/O device', 'You are attempting to input characters from (or output characters'+' to) a device that doesn`t support it. For example, it is not possible to input characters from the screen stream, or to write characters to a read-only file. A command such as INPUT #2,A$ will therefore result in this error.', 'Stream operations; OPEN #, CLOSE #, INPUT #, PRINT # etc.'),
   ('K', 'Invalid colour', 'The number specified is not an appropriate colour. Colours for INK, PAPER and BORDER range from 0 to 7, BRIGHT, FLASH, INVERSE and OVER use 0, 1, and 8.', 'INK, PAPER, BORDER, FLASH, BRIGHT, INVERSE, OVER; also after any one of the corresponding control characters.'),
   ('L', 'BREAK into program', 'BREAK pressed. This is detected between two statements. The line and '+'statement number on the report refer to the statement before BREAK was pressed, but CONTINUE goes to the statement after (allowing for any jumps to be made), so that it does not repeat any statements.', 'Any'),
   ('M', 'RAMTOP no good', 'The number specified for RAMTOP is either too big or too small.', 'CLEAR, possibly in RUN'),
   ('N', 'Statement lost', 'Jump to a statement that no longer exists.', 'RETURN, NEXT, CONTINUE'),
   ('O', 'Invalid stream', 'Trying to input from (or output to) a stream that isn`t open or that is out of range (0 to 15); or trying to open a stream that is out of range.', 'INPUT #, OPEN #, PRINT #'),
   ('P', 'FN without DEF FN', 'User-defined function used without a corresponding DEF FN in the program.', 'FN'),
   ('Q', 'Parameter error', 'Wrong number of arguments, or one of them is the wrong type (String instead of a number, or vice-versa).', 'Any function.'),
   ('R', 'File loading error', 'A file was found on disk, but could not be loaded for some reason, or would not verify.', 'VERIFY, LOAD, MERGE'),
   ('a', 'MERGE error', 'MERGE ! would not execute for some reason - either size or filetype is wrong.', 'MERGE !'),
   ('b', 'Wrong file type', 'A file of inappropriate type was specified during RAM disk operation, for instance a CODE file in LOAD !"name".', 'MERGE !, LOAD !'),
   ('c', 'CODE error', 'The size of the file would lead to an overflow past the top of the memory.', 'LOAD! file CODE'),
   ('d', 'Too many brackets', 'Too many brackets in a repeated phrase around one of the arguments.', 'PLAY'),
   ('e', 'File already exists', 'The filename specified has already been used.', 'SAVE !'),
   ('f', 'Invalid name', 'The filename specified is empty or above 10 characters in length.', 'SAVE !, ERASE !'),
   ('g', 'RAMDisk error', 'This error will never be shown, and represents a catastrophic RAM disk failure.', 'LOAD !, SAVE !, CAT !, ERASE !'),
   ('h', 'File does not exist', 'There is no file in the RAM disk that has the name specified.', 'LOAD !, MERGE !, ERASE !'),
   ('i', 'Invalid device', 'The device name following the FORMAT command does not exist or correspond to a physical device.', 'FORMAT'),
   ('j', 'Invalid baud rate', 'The baud rate for the RS232 was set to zero.', 'FORMAT LINE'),
   ('k', 'Invalid note name', 'PLAY came across a note or command that it didn'#39't recognise, or a command which was in lower case.', 'PLAY'),
   ('l', 'Number too big', 'A parameter for a command is an order of magnitude too big.', 'PLAY'),
   ('m', 'Note out of range', 'A series of sharps or flats has taken a note beyond the range of the sound chip.', 'PLAY'),
   ('n', 'Out of range', 'A parameter for a command is too big or too small. If the error is very large, error "l" results.', 'PLAY'),
   ('o', 'Too many tied notes', 'An attempt was made to tie too many notes together.', 'PLAY'),
   ('?', 'Unknown Error', 'The statement caused a jump to the ROM error routine at address 8, with an invalid error code in the system variable "ERR NR".', 'Typically occurs when a USR 8 is executed from BASIC'));

  ErrorAddressesEN: Array[0..43] Of TSpectrumError =
     ((Address:$1392; Desc:'0 OK'; Notify:True),
      (Address:$1394; Desc:'1 NEXT without FOR'; Notify:True),
      (Address:$13A4; Desc:'2 Variable not found'; Notify:True),
      (Address:$13B6; Desc:'3 Subscript wrong'; Notify:True),
      (Address:$13C5; Desc:'4 Out of memory'; Notify:True),
      (Address:$13D2; Desc:'5 Out of screen'; Notify:True),
      (Address:$13DF; Desc:'6 Number too big'; Notify:True),
      (Address:$13ED; Desc:'7 RETURN without GO SUB'; Notify:True),
      (Address:$1401; Desc:'8 End of file'; Notify:True),
      (Address:$140C; Desc:'9 STOP statement'; Notify:True),
      (Address:$141A; Desc:'A Invalid argument'; Notify:True),
      (Address:$142A; Desc:'B Integer out of range'; Notify:True),
      (Address:$143E; Desc:'C Nonsense in BASIC'; Notify:True),
      (Address:$144F; Desc:'D BREAK - CONT repeats'; Notify:True),
      (Address:$1463; Desc:'E Out of DATA'; Notify:True),
      (Address:$146E; Desc:'F Invalid filename'; Notify:True),
      (Address:$147F; Desc:'G No room for line'; Notify:True),
      (Address:$148F; Desc:'H STOP in INPUT'; Notify:True),
      (Address:$149C; Desc:'I NEXT without FOR'; Notify:True),
      (Address:$14AC; Desc:'J Invalid I/O device'; Notify:True),
      (Address:$14BE; Desc:'K Invalid colour'; Notify:True),
      (Address:$14CC; Desc:'L BREAK into program'; Notify:True),
      (Address:$14DE; Desc:'M RAMTOP no good'; Notify:True),
      (Address:$14EC; Desc:'N Statement lost'; Notify:True),
      (Address:$14FA; Desc:'O Invalid Stream'; Notify:True),
      (Address:$1508; Desc:'P FN without DEF'; Notify:True),
      (Address:$1516; Desc:'Q Parameter error'; Notify:True),
      (Address:$1525; Desc:'R File loading error'; Notify:True),
      (Address:$1392; Desc:'a MERGE error'; Notify:True),
      (Address:$1392; Desc:'b Wrong file type'; Notify:True),
      (Address:$1392; Desc:'c CODE error'; Notify:True),
      (Address:$1392; Desc:'d Too many brackets'; Notify:True),
      (Address:$1392; Desc:'e File already exists'; Notify:True),
      (Address:$1392; Desc:'f Invalid name'; Notify:True),
      (Address:$1392; Desc:'g RAMDisk error'; Notify:True),
      (Address:$1392; Desc:'h File does not exist'; Notify:True),
      (Address:$1392; Desc:'i Invalid device'; Notify:True),
      (Address:$1392; Desc:'j Invalid baud rate'; Notify:True),
      (Address:$1392; Desc:'k Invalid note name'; Notify:True),
      (Address:$1392; Desc:'l Number too big'; Notify:True),
      (Address:$1392; Desc:'m Note out of range'; Notify:True),
      (Address:$1392; Desc:'n Out of range'; Notify:True),
      (Address:$1392; Desc:'o Too many tied notes'; Notify:True),
      (Address:$1392; Desc:'? Unknown error'; Notify:True));


  ErrorsTR: Array[0..43, 1..4] of AnsiString =
  (('0', 'Baþarýlý', 'Program baþarýyla tamamlandý ya da var olan en büyük sayýlý satýrdan daha büyük bir satýra zýplandý.', 'Tümü'),
   ('1', 'FOR'#39'u olmayan NEXT', 'Kontrol deðiþkeni mevcut deðil (FOR ile kurulumu yapýlmamýþ), fakat ayný isimli bir deðiþken mevcut.', 'NEXT'),
   ('2', 'Deðiþken bulunamadý', 'Bu hata basit deðiþken için sözkonusu olduðunda, deðiþkenin FOR, LET, READ ya da INPUT komutlarý tarafýndan oluþturulmadan ya da diskten okunmadan önce kullanýlmaya baþlanmýþ olduðunu gösterir.'+' Dizi deðiþkenleri için ise deðiþken diskten okunmadan ya da DIM komutu ile kurulumu yapýlmadan kullanýldýðýný gösterir.', 'Any'),
   ('3', 'Alt öge bulunamadý', 'Aranan öge sýralý deðiþken boyutlarýnýn dýþýnda ya da alt öge sayýsý yanlýþ. Eðer alt öge negatif ise ya da 65535'#39'den büyük ise B hatasý verilir.', 'Alt Öge Deðiþkenleri, harf dizinleri'),
   ('4', 'Yetersiz bellek', 'Yapmak istenilen iþlem için yeterli bellek kalmadý. Eðer yorumlayýcý bu durumda takýlýp kaldýysa, CLEAR komutunu kullanarak bellekte tutulan deðiþkenlerin temizlenmesi gerekmektedir.', 'LET, INPUT, FOR, DIM, GO SUB, LOAD, MERGE. Bazý durumlarda hesaplama anýnda.'),
   ('5', 'Ekran alaný dýþýnda', 'Bir INPUT komutu 23 satýrdan fazla metin oluþturmaya çalýþtý. Ayrýca PRINT AT 22,xx kullanýldýðýnda da oluþur.', 'INPUT, PRINT AT'),
   ('6', 'Sayý çok büyük ', 'Hesaplamalar sonucunda 10 üzeri 38'#39'den daha büyük bir rakam oluþtu.', 'Tüm aritmatik hesaplar'),
   ('7', 'GO SUB'#39'u olmayan RETURN', 'Çalýþtýrýlan GO SUB sayýsýndan bir fazla RETURN iþletildi.', 'RETURN'),
   ('8', 'Bilinmeyen hata', 'Yorumlayýcý iþleyemeyeceði bir komut ya da ifade ile karþýlaþtý. Bu durumda program akýþý durur.', 'Herhangi bir ifade ya da komut.'),
   ('9', 'STOP komutu', 'CONTINUE komutu STOP'#39'u tekrarlamayacak ve bir sonraki ifadeden devam edecektir.', 'STOP'),
   ('A', 'Geçersiz argüman', 'Fonksiyon için verilen argüman uygun deðil.', 'SQR, LN, ASC, ACS, USR (dizi deðiþkeni ile)'),
   ('B', 'Tam sayý kapsam dýþýnda', 'Bir tam sayý gerektiðinde, küsüratlý bir argüman en yakýn tam sayýya yuvarlanýr. Eðer bu beklenen aralýðýn dýþýnda ise bu hata ortaya çýkar. Sýra deðiþken eriþimi için bkz: HATA 3.', 'RUN, RANDOMIZE, POKE, DIM, GO TO, GO SUB, LIST, LLIST, PAUSE, PLOT, CHR$, PEEK, USR (nümerik argüman ile)'),
   ('C', 'BASIC'#39'de anlamý yok', 'Yazýlan metin anlamlý bir ifade oluþturmuyor. Ayný zamanda bir fonksiyon için kullanýlan çok yanlýþ bir argüman hakkýnda da bu hata üretilebilir.', 'VAL, VAL$'),
   ('D', 'BREAK - CONTINUE son komutu tekrarlar', 'Bir çevresel iþlem sýrasýnda BREAK tuþuna basýldý. Bu raporun ardýndan kullanýlan CONTINUE komutu ifadeyi tekrarlayacaktýr. L raporu ile karþýlaþtýrýn.', 'LOAD, SAVE, VERIFY, MERGE. Ayný zamanda ekrandaki "Scroll?" sorusuna N, BREAK ya da boþluk tuþuna basarak cevap verdiðinizde.'),
   ('E', 'DATA bitti', 'DATA listesininde bulunan veriler tükenmesine karþýn program READ ile okumaya devam etmeyi denedi.', 'READ'),
   ('F', 'Geçersiz dosya adý', 'SAVE ile kullandýðýnýz dosya adý dosya sisteminizde bir anlam ifade etmiyor.', 'SAVE'),
   ('G', 'Satýr için yer yok', 'Program hafýzasýnda yeni bir satýrý kaydedecek kadar yer kalmadý.', 'Programa yeni bir satýr girerken.'),
   ('H', 'INPUTda STOP kullanýldý', 'INPUT sýrasýnda girilen veri STOP komutu ile baþlýyor. Hata raporu 9'#39'un aksine, CONTINUE normal davranýr ve INPUT ifadesini tekrar eder.', 'INPUT'),
   ('I', 'NEXT olmadan FOR', 'Hiç tekrar edilmeyecek bir FOR döngüsü ayarlandý (örn. FOR n=1 TO 0) ve zýplanmasý gereken NEXT komutu bulunamadý.', 'FOR'),
   ('J', 'Geçersiz G/Ç aygýtý', 'Metin giriþ (INPUT) çýkýþý (OUT) desteklemeyen bir aygýta karakter yazmaya ya da okumaya çalýþtýnýz.'+' Örneðin, ekran akýþýndan karakter okumak mümkün deðildir ya da sadece okunabilir bir dosyaya yeni karakterler ekleyemezsiniz. Bu durumda INPUT #2,A$ benzeri bir ifade bu hataya sebep olur.' , 'Akýþ operasyonlarý; OPEN #, CLOSE #, INPUT #, PRINT # vb.'),
   ('K', 'Geçersiz Renk', 'Verilen rakam renk için geçersiz. INK, PAPER ve BORDER komutlarý 0 ile 7 arasýndaki, BRIGHT, FLASH, INVERSE ve OVER sadece 0, 1, ve 8 rakamlarýný destekler.', 'INK, PAPER, BORDER, FLASH, BRIGHT, INVERSE, OVER; ayrýca ayný iþi yapan kontrol karakterlerinden sonra.'),
   ('L', 'Program BREAK ile kesildi', 'BREAK tuþuna basýldý. Kesme isteði iki ifade arasýnda oluþtu. Satýr ve '+'ifade numarasý BREAK tuþuna basýlmadan önceki ifadeye aittir, fakat CONTINUE sonraki ifadeden devam edecektir (zýplamalarýn gerçekleþmesine izin verecek þekilde), yani ifadeler tekrar edilmez.', 'Tümü'),
   ('M', 'RAMTOP iþe yaramýyor', 'RAMTOP için verilmiþ olan adres sayýsý çok büyük ya da çok küçük.', 'CLEAR, bazen RUN'),
   ('N', 'Ýfade kayýp', 'Artýk geçerli olmayan bir ifadeye zýplandý.', 'RETURN, NEXT, CONTINUE'),
   ('O', 'Geçersiz akýþ', 'Geçerli akýþ kanallarý olan 0 ile 15 dýþýnda bir kanala ya da açýk olmayan bir kanala yazýlmaya çalýþýldý, ya da aralýk dýþýnda bir kanal açýlmaya çalýþýldý.', 'INPUT #, OPEN #, PRINT #'),
   ('P', 'DEF FN olmadan FN', 'Kullanýcý tarafýndan tanýmlanmýþ bir fonksiyon (FN) önceden tanýmlanmadan (DEF FN) programda kullanýldý.', 'FN'),
   ('Q', 'Parametre hatasý', 'Yanlýþ sayýda argüman ya da argümanlardan birisinin tipi yanlýþ (rakam yerine harf dizisi ya da tersi).', 'Herhangi bir fonksiyon.'),
   ('R', 'Dosya yükleme hatasý', 'Bir dosya disk ya da kasette bulundu fakat yüklenme sýrasýnda bir hata oluþtu ya da kontrol hatasý verdi.', 'VERIFY, LOAD, MERGE'),
   ('a', 'MERGE hatasý', 'MERGE ! bir sebepten doðru çalýþamadý. Dosya tipi ya da boyutu hatalý.', 'MERGE !'),
   ('b', 'Dosya tipi yanlýþ', 'Ram disk iþlemi sýrasýnda yanlýþ dosya tipi verilmiþ. Örneðin LOAD !"name" komutu için CODE dosyasý bulunmasý.', 'MERGE !, LOAD !'),
   ('c', 'CODE hatasý', 'Dosya boyutu bellek sýnýrlarýný aþtý.', 'LOAD! file CODE'),
   ('d', 'Çok fazla parantez', 'Argümanlarýn birinin etrafýnda çok fazla parantez mevcut.', 'PLAY'),
   ('e', 'Dosya zaten mevcut', 'Verilen dosya ismi zaten bulunuyor.', 'SAVE !'),
   ('f', 'Geçersiz isim', 'Dosya ismi yanlýþ ya da 10 karakterden fazla.', 'SAVE !, ERASE !'),
   ('g', 'RAMDisk hatasý', 'Bu hata asla gösterilmeyecektir, RAM hatasýný gösterir.', 'LOAD !, SAVE !, CAT !, ERASE !'),
   ('h', 'Dosya bulunamadý', 'RamDiskte verilen dosya ismi bulunmuyor.', 'LOAD !, MERGE !, ERASE !'),
   ('i', 'Geçersiz aygýt', 'FORMAT komutundan sonra verilmiþ aygýt adý mevcut deðil ya da bir fiziksel aygýta ait deðil.', 'FORMAT'),
   ('j', 'Geçersiz Baud', 'RS232 aygýtý için baud hýzý sýfýr olarak ayarlanmýþ.', 'FORMAT LINE'),
   ('k', 'Geçersiz nota adý', 'PLAY komutu tanýmadýðý bir harf ile ya da küçük harfli bir karakterle karþýlaþtý.', 'PLAY'),
   ('l', 'Sayý çok büyük', 'Komut için kullanýlan parametrede verilen rakam çok büyük.', 'PLAY'),
   ('m', 'Nota aralýk dýþýnda', 'Bir dizi diyez ya da bemol notayý ses çipinin dýþýna taþýdý.', 'PLAY'),
   ('n', 'Aralýk dýþýnda', 'Bir parametre çok büyük ya da küçük. Eðer hata çok büyükse hata l ortaya çýkar.', 'PLAY'),
   ('o', 'Çok fazla baðlý nota', 'Çok fazla nota birbirine baðlanmaya çalýþtý.', 'PLAY'),
   ('?', 'Bilinmeyen hata', 'Bir ifade adres 8'#39'deki rom rutinine zýplamaya sebep oldu fakat ERR NR sistem deðiþkeninde geçersiz bir rakam mevcuttu.', 'Genellikle BASIC'#39' USR 8 komutu iþletildiðinde oluþur'));

  ErrorAddressesTR: Array[0..43] Of TSpectrumError =
     ((Address:$1392; Desc:'0 BAÞARILI'; Notify:True),
      (Address:$1394; Desc:'1 FOR'#39'u olmayan NEXT'; Notify:True),
      (Address:$13A4; Desc:'2 Deðiþken bulunamadý'; Notify:True),
      (Address:$13B6; Desc:'3 Alt öge bulunamadý'; Notify:True),
      (Address:$13C5; Desc:'4 Yetersiz bellek'; Notify:True),
      (Address:$13D2; Desc:'5 Ekran alaný dýþýnda'; Notify:True),
      (Address:$13DF; Desc:'6 Sayý çok büyük'; Notify:True),
      (Address:$13ED; Desc:'7 GO SUB'#39'u olmayan RETURN'; Notify:True),
      (Address:$1401; Desc:'8 Dosya sonu'; Notify:True),
      (Address:$140C; Desc:'9 STOP komutu'; Notify:True),
      (Address:$141A; Desc:'A Geçersiz argüman'; Notify:True),
      (Address:$142A; Desc:'B Tamsayý kapsam dýþýnda'; Notify:True),
      (Address:$143E; Desc:'C BASIC'#39'de anlamý yok'; Notify:True),
      (Address:$144F; Desc:'D BREAK - CONT son komutu tekrarlar'; Notify:True),
      (Address:$1463; Desc:'E DATA bitti'; Notify:True),
      (Address:$146E; Desc:'F Geçersiz dosya adý'; Notify:True),
      (Address:$147F; Desc:'G Satýr için yer yok'; Notify:True),
      (Address:$148F; Desc:'H INPUTda STOP kullanýldý'; Notify:True),
      (Address:$149C; Desc:'I NEXT olmadan FOR'; Notify:True),
      (Address:$14AC; Desc:'J Geçersiz G/Ç aygýtý'; Notify:True),
      (Address:$14BE; Desc:'K Geçersiz Renk'; Notify:True),
      (Address:$14CC; Desc:'L Program BREAK ile kesildi'; Notify:True),
      (Address:$14DE; Desc:'M RAMTOP iþe yaramýyor'; Notify:True),
      (Address:$14EC; Desc:'N Ýfade kayýp'; Notify:True),
      (Address:$14FA; Desc:'O Geçersiz akýþ'; Notify:True),
      (Address:$1508; Desc:'P DEF FN olmadan FN'; Notify:True),
      (Address:$1516; Desc:'Q Parametre hatasý'; Notify:True),
      (Address:$1525; Desc:'R Dosya yükleme hatasý'; Notify:True),
      (Address:$1392; Desc:'a MERGE hatasý'; Notify:True),
      (Address:$1392; Desc:'b Yanlýþ dosya tipi'; Notify:True),
      (Address:$1392; Desc:'c CODE hatasý'; Notify:True),
      (Address:$1392; Desc:'d Çok fazla parantez'; Notify:True),
      (Address:$1392; Desc:'e Dosya zaten mevcut'; Notify:True),
      (Address:$1392; Desc:'f Geçersiz isim'; Notify:True),
      (Address:$1392; Desc:'g RAMDisk hatasý'; Notify:True),
      (Address:$1392; Desc:'h Dosya bulunamadý'; Notify:True),
      (Address:$1392; Desc:'i Geçersiz aygýt'; Notify:True),
      (Address:$1392; Desc:'j Geçersiz baud'; Notify:True),
      (Address:$1392; Desc:'k Geçersiz nota adý'; Notify:True),
      (Address:$1392; Desc:'l Sayý çok büyük'; Notify:True),
      (Address:$1392; Desc:'m Nota aralýk dýþýnda'; Notify:True),
      (Address:$1392; Desc:'n Aralýk dýþýnda'; Notify:True),
      (Address:$1392; Desc:'o Çok fazla baðlý nota'; Notify:True),
      (Address:$1392; Desc:'? Bilinmeyen hata'; Notify:True));


  ErrorsDE: Array[0..43, 1..4] of AnsiString =
  (('0', 'Erfolgreich', 'Programm erfolgreich beendet oder Sprung zu einer Zeile größer als die höchste existierende Zeilennummer.', 'Jeder'),
   ('1', 'NEXT ohne FOR', 'Die Kontrollvariable existiert nicht (wurde nicht mit FOR eingerichtet), aber es gibt eine Variable mit demselben Namen.', 'NEXT'),
   ('2', 'Variable nicht gefunden', 'Bei einfachen Variablen zeigt dies an, dass die Variable verwendet wurde, bevor sie durch FOR, LET, READ oder INPUT zugewiesen oder geladen wurde.'+' Bei Array-Variablen bedeutet es, dass das Array verwendet wurde, bevor es geladen oder mit DIM dimensioniert wurde.', 'Beliebig'),
   ('3', 'Index falsch', 'Das angeforderte Element liegt außerhalb der Array-Dimensionen oder die Anzahl der Indizes ist falsch. Wenn der Index negativ oder größer als 65535 ist, wird Fehler B ausgegeben.', 'Indizierte Variablen, Strings'),
   ('4', 'Zu wenig Speicher', 'Nicht genügend Speicher für den gewünschten Vorgang. Wenn der Interpreter hängt, muss der Speicher mit dem Befehl CLEAR von Variablen bereinigt werden.', 'LET, INPUT, FOR, DIM, GO SUB, LOAD, MERGE. Manchmal während der Berechnung.'),
   ('5', 'Außerhalb des Bildschirms', 'Ein INPUT-Befehl versuchte, mehr als 23 Zeilen Text zu erzeugen. Tritt auch bei Verwendung von PRINT AT 22,xx auf.', 'INPUT, PRINT AT'),
   ('6', 'Zahl zu groß', 'Das Ergebnis der Berechnungen ist größer als 10 hoch 38.', 'Alle arithmetischen Berechnungen'),
   ('7', 'RETURN ohne GO SUB', 'Es wurde ein RETURN mehr ausgeführt als GO SUB-Befehle.', 'RETURN'),
   ('8', 'Unbekannter Fehler', 'Der Interpreter stieß auf einen Befehl oder Ausdruck, den er nicht verarbeiten kann. In diesem Fall stoppt der Programmablauf.', 'Jeder Ausdruck oder Befehl.'),
   ('9', 'STOP-Anweisung', 'Der Befehl CONTINUE wiederholt STOP nicht, sondern fährt mit der nächsten Anweisung fort.', 'STOP'),
   ('A', 'Ungültiges Argument', 'Das für die Funktion angegebene Argument ist ungeeignet.', 'SQR, LN, ASC, ACS, USR (mit Array-Variable)'),
   ('B', 'Ganzzahl außerhalb des Bereichs', 'Wenn eine Ganzzahl erforderlich ist, wird ein Gleitkomma-Argument zur nächsten Ganzzahl gerundet. Liegt diese außerhalb des erwarteten Bereichs, tritt dieser Fehler auf. Für Array-Zugriff siehe: FEHLER 3.', 'RUN, RANDOMIZE, POKE, DIM, GO TO, GO SUB, LIST, LLIST, PAUSE, PLOT, CHR$, PEEK, USR (mit numerischem Argument)'),
   ('C', 'Unsinn in BASIC', 'Der geschriebene Text ergibt keinen sinnvollen Ausdruck. Dieser Fehler kann auch bei einem völlig falschen Argument für eine Funktion erzeugt werden.', 'VAL, VAL$'),
   ('D', 'BREAK - CONTINUE wiederholt', 'Während einer peripheren Operation wurde die BREAK-Taste gedrückt. Der nach dieser Meldung verwendete CONTINUE-Befehl wiederholt die Anweisung. Vergleiche mit Bericht L.', 'LOAD, SAVE, VERIFY, MERGE. Auch wenn auf die Frage "Scroll?" mit N, BREAK oder Leertaste geantwortet wird.'),
   ('E', 'Keine DATA mehr', 'Das Programm versuchte mit READ weiterzulesen, obwohl die Daten in der DATA-Liste erschöpft waren.', 'READ'),
   ('F', 'Ungültiger Dateiname', 'Der mit SAVE verwendete Dateiname hat im Dateisystem keine Bedeutung.', 'SAVE'),
   ('G', 'Kein Platz für Zeile', 'Im Programmspeicher ist kein Platz mehr, um eine neue Zeile zu speichern.', 'Bei Eingabe einer neuen Programmzeile.'),
   ('H', 'STOP in INPUT', 'Die während INPUT eingegebenen Daten beginnen mit dem STOP-Befehl. Im Gegensatz zu Fehlerbericht 9 verhält sich CONTINUE normal und wiederholt die INPUT-Anweisung.', 'INPUT'),
   ('I', 'FOR ohne NEXT', 'Es wurde eine FOR-Schleife eingerichtet, die nie wiederholt wird (z.B. FOR n=1 TO 0), und der entsprechende NEXT-Befehl zum Springen wurde nicht gefunden.', 'FOR'),
   ('J', 'Ungültiges E/A-Gerät', 'Sie haben versucht, auf ein Gerät zu schreiben oder von ihm zu lesen, das Texteingabe (INPUT) oder -ausgabe (OUT) nicht unterstützt.'+' Zum Beispiel ist es nicht möglich, Zeichen vom Bildschirm-Stream zu lesen, oder Sie können keine Zeichen an eine schreibgeschützte Datei anhängen. In diesem Fall verursacht ein Ausdruck wie INPUT #2,A$ diesen Fehler.', 'Stream-Operationen; OPEN #, CLOSE #, INPUT #, PRINT # usw.'),
   ('K', 'Ungültige Farbe', 'Die angegebene Zahl ist für die Farbe ungültig. Die Befehle INK, PAPER und BORDER unterstützen 0 bis 7; BRIGHT, FLASH, INVERSE und OVER unterstützen nur 0, 1 und 8.', 'INK, PAPER, BORDER, FLASH, BRIGHT, INVERSE, OVER; auch nach Steuerzeichen, die dasselbe tun.'),
   ('L', 'Programm durch BREAK unterbrochen', 'Die BREAK-Taste wurde gedrückt. Die Unterbrechungsanforderung erfolgte zwischen zwei Anweisungen. Zeile und '+'Anweisungsnummer gehören zur Anweisung VOR dem Drücken von BREAK, aber CONTINUE fährt mit der nächsten Anweisung fort (erlaubt Sprünge), d.h. Anweisungen werden nicht wiederholt.', 'Jede'),
   ('M', 'RAMTOP ungültig', 'Die für RAMTOP angegebene Adresszahl ist zu groß oder zu klein.', 'CLEAR, manchmal RUN'),
   ('N', 'Anweisung verloren', 'Es wurde zu einer Anweisung gesprungen, die nicht mehr gültig ist.', 'RETURN, NEXT, CONTINUE'),
   ('O', 'Ungültiger Stream', 'Es wurde versucht, auf einen Kanal außerhalb der gültigen Streams 0 bis 15 oder auf einen nicht geöffneten Kanal zu schreiben, oder es wurde versucht, einen Kanal außerhalb des Bereichs zu öffnen.', 'INPUT #, OPEN #, PRINT #'),
   ('P', 'FN ohne DEF FN', 'Eine benutzerdefinierte Funktion (FN) wurde im Programm verwendet, ohne zuvor definiert worden zu sein (DEF FN).', 'FN'),
   ('Q', 'Parameterfehler', 'Falsche Anzahl von Argumenten oder der Typ eines Arguments ist falsch (String statt Zahl oder umgekehrt).', 'Jede Funktion.'),
   ('R', 'Ladefehler', 'Eine Datei wurde auf Diskette oder Kassette gefunden, aber beim Laden trat ein Fehler auf oder die Prüfung schlug fehl.', 'VERIFY, LOAD, MERGE'),
   ('a', 'MERGE-Fehler', 'MERGE ! funktionierte aus irgendeinem Grund nicht richtig. Dateityp oder Größe fehlerhaft.', 'MERGE !'),
   ('b', 'Falscher Dateityp', 'Falscher Dateityp während einer Ram-Disk-Operation. Z.B. Auffinden einer CODE-Datei für den Befehl LOAD !"name".', 'MERGE !, LOAD !'),
   ('c', 'CODE-Fehler', 'Dateigröße überschreitet Speichergrenzen.', 'LOAD! file CODE'),
   ('d', 'Zu viele Klammern', 'Es gibt zu viele Klammern um eines der Argumente.', 'PLAY'),
   ('e', 'Datei existiert bereits', 'Der angegebene Dateiname existiert bereits.', 'SAVE !'),
   ('f', 'Ungültiger Name', 'Dateiname ist falsch oder länger als 10 Zeichen.', 'SAVE !, ERASE !'),
   ('g', 'RAMDisk-Fehler', 'Dieser Fehler wird niemals angezeigt, er zeigt einen RAM-Fehler an.', 'LOAD !, SAVE !, CAT !, ERASE !'),
   ('h', 'Datei nicht gefunden', 'Der angegebene Dateiname wurde auf der RamDisk nicht gefunden.', 'LOAD !, MERGE !, ERASE !'),
   ('i', 'Ungültiges Gerät', 'Der nach dem Befehl FORMAT angegebene Gerätename existiert nicht oder gehört nicht zu einem physischen Gerät.', 'FORMAT'),
   ('j', 'Ungültige Baudrate', 'Die Baudrate für das RS232-Gerät ist auf Null gesetzt.', 'FORMAT LINE'),
   ('k', 'Ungültiger Notenname', 'Der Befehl PLAY stieß auf einen unbekannten Buchstaben oder ein Kleinbuchstaben-Zeichen.', 'PLAY'),
   ('l', 'Zahl zu groß', 'Die im Parameter für den Befehl verwendete Zahl ist zu groß.', 'PLAY'),
   ('m', 'Note außer Reichweite', 'Eine Reihe von Kreuzen oder Bs hat die Note aus dem Bereich des Soundchips geschoben.', 'PLAY'),
   ('n', 'Außerhalb des Bereichs', 'Ein Parameter ist zu groß oder zu klein. Wenn der Fehler sehr groß ist, tritt Fehler l auf.', 'PLAY'),
   ('o', 'Zu viele gebundene Noten', 'Es wurde versucht, zu viele Noten miteinander zu verbinden.', 'PLAY'),
   ('?', 'Unbekannter Fehler', 'Eine Anweisung verursachte einen Sprung zur ROM-Routine bei Adresse 8, aber in der Systemvariable ERR NR war eine ungültige Zahl vorhanden.', 'Tritt normalerweise auf, wenn der Befehl BASIC'#39' USR 8 ausgeführt wird'));

  ErrorAddressesDE: Array[0..43] Of TSpectrumError =
     ((Address:$1392; Desc:'0 ERFOLGREICH'; Notify:True),
      (Address:$1394; Desc:'1 NEXT ohne FOR'; Notify:True),
      (Address:$13A4; Desc:'2 Variable nicht gefunden'; Notify:True),
      (Address:$13B6; Desc:'3 Index falsch'; Notify:True),
      (Address:$13C5; Desc:'4 Zu wenig Speicher'; Notify:True),
      (Address:$13D2; Desc:'5 Außerhalb des Bildschirms'; Notify:True),
      (Address:$13DF; Desc:'6 Zahl zu groß'; Notify:True),
      (Address:$13ED; Desc:'7 RETURN ohne GO SUB'; Notify:True),
      (Address:$1401; Desc:'8 Dateiende'; Notify:True),
      (Address:$140C; Desc:'9 STOP-Anweisung'; Notify:True),
      (Address:$141A; Desc:'A Ungültiges Argument'; Notify:True),
      (Address:$142A; Desc:'B Ganzzahl außerhalb des Bereichs'; Notify:True),
      (Address:$143E; Desc:'C Unsinn in BASIC'; Notify:True),
      (Address:$144F; Desc:'D BREAK - CONT wiederholt'; Notify:True),
      (Address:$1463; Desc:'E Keine DATA mehr'; Notify:True),
      (Address:$146E; Desc:'F Ungültiger Dateiname'; Notify:True),
      (Address:$147F; Desc:'G Kein Platz für Zeile'; Notify:True),
      (Address:$148F; Desc:'H STOP in INPUT'; Notify:True),
      (Address:$149C; Desc:'I FOR ohne NEXT'; Notify:True),
      (Address:$14AC; Desc:'J Ungültiges E/A-Gerät'; Notify:True),
      (Address:$14BE; Desc:'K Ungültige Farbe'; Notify:True),
      (Address:$14CC; Desc:'L Programm durch BREAK unterbrochen'; Notify:True),
      (Address:$14DE; Desc:'M RAMTOP ungültig'; Notify:True),
      (Address:$14EC; Desc:'N Anweisung verloren'; Notify:True),
      (Address:$14FA; Desc:'O Ungültiger Stream'; Notify:True),
      (Address:$1508; Desc:'P FN ohne DEF FN'; Notify:True),
      (Address:$1516; Desc:'Q Parameterfehler'; Notify:True),
      (Address:$1525; Desc:'R Ladefehler'; Notify:True),
      (Address:$1392; Desc:'a MERGE-Fehler'; Notify:True),
      (Address:$1392; Desc:'b Falscher Dateityp'; Notify:True),
      (Address:$1392; Desc:'c CODE-Fehler'; Notify:True),
      (Address:$1392; Desc:'d Zu viele Klammern'; Notify:True),
      (Address:$1392; Desc:'e Datei existiert bereits'; Notify:True),
      (Address:$1392; Desc:'f Ungültiger Name'; Notify:True),
      (Address:$1392; Desc:'g RAMDisk-Fehler'; Notify:True),
      (Address:$1392; Desc:'h Datei nicht gefunden'; Notify:True),
      (Address:$1392; Desc:'i Ungültiges Gerät'; Notify:True),
      (Address:$1392; Desc:'j Ungültige Baudrate'; Notify:True),
      (Address:$1392; Desc:'k Ungültiger Notenname'; Notify:True),
      (Address:$1392; Desc:'l Zahl zu groß'; Notify:True),
      (Address:$1392; Desc:'m Note außer Reichweite'; Notify:True),
      (Address:$1392; Desc:'n Außerhalb des Bereichs'; Notify:True),
      (Address:$1392; Desc:'o Zu viele gebundene Noten'; Notify:True),
      (Address:$1392; Desc:'? Unbekannter Fehler'; Notify:True));



  ErrorsES: Array[0..43, 1..4] of AnsiString =
  (('0', 'Correcto', 'El programa ha terminado correctamente o se ha saltado a una línea mayor que la última existente.', 'Cualquiera'),
   ('1', 'NEXT sin FOR', 'La variable de control no existe (no se ha configurado con FOR), pero existe una variable con el mismo nombre.', 'NEXT'),
   ('2', 'Variable no encontrada', 'Para una variable simple, indica que se está utilizando antes de ser creada por FOR, LET, READ o INPUT o leída desde disco.'+' Para variables de arreglo, indica que se usa antes de ser leída o dimensionada con DIM.', 'Cualquiera'),
   ('3', 'Subíndice indebido', 'El elemento buscado está fuera de las dimensiones del arreglo o el número de subíndices es incorrecto. Si el subíndice es negativo o mayor que 65535, se da el error B.', 'Variables de arreglo, cadenas'),
   ('4', 'Memoria llena', 'No queda suficiente memoria para la operación deseada. Si el intérprete se queda colgado, se debe usar el comando CLEAR para limpiar las variables de la memoria.', 'LET, INPUT, FOR, DIM, GO SUB, LOAD, MERGE. A veces durante el cálculo.'),
   ('5', 'Fuera de pantalla', 'Un comando INPUT intentó generar más de 23 líneas de texto. También ocurre al usar PRINT AT 22,xx.', 'INPUT, PRINT AT'),
   ('6', 'Número demasiado grande', 'El resultado de los cálculos es un número mayor que 10 elevado a 38.', 'Todos los cálculos aritméticos'),
   ('7', 'RETURN sin GO SUB', 'Se ha ejecutado un RETURN más que el número de comandos GO SUB ejecutados.', 'RETURN'),
   ('8', 'Error desconocido', 'El intérprete encontró un comando o expresión que no puede procesar. En este caso, el flujo del programa se detiene.', 'Cualquier expresión o comando.'),
   ('9', 'Comando STOP', 'El comando CONTINUE no repetirá el STOP, sino que continuará desde la siguiente instrucción.', 'STOP'),
   ('A', 'Argumento inválido', 'El argumento dado para la función no es adecuado.', 'SQR, LN, ASC, ACS, USR (con variable de arreglo)'),
   ('B', 'Entero fuera de rango', 'Cuando se requiere un entero, un argumento decimal se redondea al entero más cercano. Si este está fuera del rango esperado, aparece este error. Para acceso a arreglos ver: ERROR 3.', 'RUN, RANDOMIZE, POKE, DIM, GO TO, GO SUB, LIST, LLIST, PAUSE, PLOT, CHR$, PEEK, USR (con argumento numérico)'),
   ('C', 'Sin sentido en BASIC', 'El texto escrito no forma una expresión con sentido. También puede generarse este error por un argumento muy incorrecto para una función.', 'VAL, VAL$'),
   ('D', 'BREAK - CONTINUE repite', 'Se pulsó la tecla BREAK durante una operación de periférico. El comando CONTINUE tras este reporte repetirá la instrucción. Comparar con el reporte L.', 'LOAD, SAVE, VERIFY, MERGE. También al responder N, BREAK o espacio a la pregunta "Scroll?".'),
   ('E', 'Fin de DATA', 'El programa intentó leer con READ aunque los datos en la lista DATA se habían agotado.', 'READ'),
   ('F', 'Nombre de archivo inválido', 'El nombre de archivo usado con SAVE no tiene significado en el sistema de archivos.', 'SAVE'),
   ('G', 'Sin espacio para la línea', 'No queda espacio en la memoria del programa para guardar una nueva línea.', 'Al ingresar una nueva línea de programa.'),
   ('H', 'STOP en INPUT', 'Los datos ingresados durante INPUT comienzan con el comando STOP. A diferencia del error 9, CONTINUE se comporta normalmente y repite la instrucción INPUT.', 'INPUT'),
   ('I', 'FOR sin NEXT', 'Se configuró un bucle FOR que nunca se repetirá (ej. FOR n=1 TO 0) y no se encontró el comando NEXT al que saltar.', 'FOR'),
   ('J', 'Dispositivo E/S inválido', 'Intentó escribir o leer caracteres en un dispositivo que no soporta entrada (INPUT) o salida (OUT) de texto.'+' Por ejemplo, no es posible leer caracteres del flujo de pantalla o no puede añadir caracteres a un archivo de solo lectura. En este caso, una expresión como INPUT #2,A$ causa este error.', 'Operaciones de flujo; OPEN #, CLOSE #, INPUT #, PRINT # etc.'),
   ('K', 'Color inválido', 'El número dado para el color es inválido. Los comandos INK, PAPER y BORDER soportan entre 0 y 7; BRIGHT, FLASH, INVERSE y OVER solo soportan 0, 1 y 8.', 'INK, PAPER, BORDER, FLASH, BRIGHT, INVERSE, OVER; también tras caracteres de control que hacen lo mismo.'),
   ('L', 'Programa interrumpido por BREAK', 'Se pulsó la tecla BREAK. La solicitud de interrupción ocurrió entre dos instrucciones. La línea y '+'número de instrucción pertenecen a la instrucción ANTERIOR a pulsar BREAK, pero CONTINUE continuará desde la siguiente instrucción (permitiendo saltos), es decir, las instrucciones no se repiten.', 'Cualquiera'),
   ('M', 'RAMTOP inválido', 'La dirección dada para RAMTOP es demasiado grande o demasiado pequeña.', 'CLEAR, a veces RUN'),
   ('N', 'Instrucción perdida', 'Se saltó a una instrucción que ya no es válida.', 'RETURN, NEXT, CONTINUE'),
   ('O', 'Flujo inválido', 'Se intentó escribir en un canal fuera de los flujos válidos 0 a 15 o en un canal no abierto, o se intentó abrir un canal fuera de rango.', 'INPUT #, OPEN #, PRINT #'),
   ('P', 'FN sin DEF FN', 'Una función definida por el usuario (FN) se usó en el programa sin haber sido definida previamente (DEF FN).', 'FN'),
   ('Q', 'Error de parámetro', 'Número incorrecto de argumentos o el tipo de uno de ellos es incorrecto (cadena en lugar de número o viceversa).', 'Cualquier función.'),
   ('R', 'Error de carga de archivo', 'Se encontró un archivo en disco o cinta pero ocurrió un error durante la carga o falló la verificación.', 'VERIFY, LOAD, MERGE'),
   ('a', 'Error de MERGE', '¡MERGE ! no funcionó correctamente por alguna razón. Tipo o tamaño de archivo incorrecto.', 'MERGE !'),
   ('b', 'Tipo de archivo incorrecto', 'Tipo de archivo incorrecto durante operación de disco RAM. Ej. encontrar un archivo CODE para el comando LOAD !"name".', 'MERGE !, LOAD !'),
   ('c', 'Error de CODE', 'El tamaño del archivo excede los límites de memoria.', 'LOAD! file CODE'),
   ('d', 'Demasiados paréntesis', 'Hay demasiados paréntesis alrededor de uno de los argumentos.', 'PLAY'),
   ('e', 'El archivo ya existe', 'El nombre de archivo dado ya existe.', 'SAVE !'),
   ('f', 'Nombre inválido', 'El nombre de archivo es incorrecto o tiene más de 10 caracteres.', 'SAVE !, ERASE !'),
   ('g', 'Error de RAMDisk', 'Este error nunca se mostrará, indica un fallo de RAM.', 'LOAD !, SAVE !, CAT !, ERASE !'),
   ('h', 'Archivo no encontrado', 'El nombre de archivo dado no se encuentra en el RAMDisk.', 'LOAD !, MERGE !, ERASE !'),
   ('i', 'Dispositivo inválido', 'El nombre de dispositivo tras el comando FORMAT no existe o no pertenece a un dispositivo físico.', 'FORMAT'),
   ('j', 'Baudios inválidos', 'La velocidad de baudios para el dispositivo RS232 está establecida en cero.', 'FORMAT LINE'),
   ('k', 'Nombre de nota inválido', 'El comando PLAY encontró una letra desconocida o un carácter en minúscula.', 'PLAY'),
   ('l', 'Número demasiado grande', 'El número usado en el parámetro para el comando es demasiado grande.', 'PLAY'),
   ('m', 'Nota fuera de rango', 'Una serie de sostenidos o bemoles ha llevado la nota fuera del rango del chip de sonido.', 'PLAY'),
   ('n', 'Fuera de rango', 'Un parámetro es demasiado grande o pequeño. Si el error es muy grande, aparece el error l.', 'PLAY'),
   ('o', 'Demasiadas notas ligadas', 'Se intentaron ligar demasiadas notas entre sí.', 'PLAY'),
   ('?', 'Error desconocido', 'Una instrucción causó un salto a la rutina ROM en dirección 8 pero había un número inválido en la variable del sistema ERR NR.', 'Generalmente ocurre al ejecutar el comando BASIC'#39' USR 8'));

  ErrorAddressesES: Array[0..43] Of TSpectrumError =
     ((Address:$1392; Desc:'0 CORRECTO'; Notify:True),
      (Address:$1394; Desc:'1 NEXT sin FOR'; Notify:True),
      (Address:$13A4; Desc:'2 Variable no encontrada'; Notify:True),
      (Address:$13B6; Desc:'3 Subíndice indebido'; Notify:True),
      (Address:$13C5; Desc:'4 Memoria llena'; Notify:True),
      (Address:$13D2; Desc:'5 Fuera de pantalla'; Notify:True),
      (Address:$13DF; Desc:'6 Número demasiado grande'; Notify:True),
      (Address:$13ED; Desc:'7 RETURN sin GO SUB'; Notify:True),
      (Address:$1401; Desc:'8 Fin de archivo'; Notify:True),
      (Address:$140C; Desc:'9 Comando STOP'; Notify:True),
      (Address:$141A; Desc:'A Argumento inválido'; Notify:True),
      (Address:$142A; Desc:'B Entero fuera de rango'; Notify:True),
      (Address:$143E; Desc:'C Sin sentido en BASIC'; Notify:True),
      (Address:$144F; Desc:'D BREAK - CONT repite'; Notify:True),
      (Address:$1463; Desc:'E Fin de DATA'; Notify:True),
      (Address:$146E; Desc:'F Nombre de archivo inválido'; Notify:True),
      (Address:$147F; Desc:'G Sin espacio para la línea'; Notify:True),
      (Address:$148F; Desc:'H STOP en INPUT'; Notify:True),
      (Address:$149C; Desc:'I FOR sin NEXT'; Notify:True),
      (Address:$14AC; Desc:'J Dispositivo E/S inválido'; Notify:True),
      (Address:$14BE; Desc:'K Color inválido'; Notify:True),
      (Address:$14CC; Desc:'L Programa interrumpido por BREAK'; Notify:True),
      (Address:$14DE; Desc:'M RAMTOP inválido'; Notify:True),
      (Address:$14EC; Desc:'N Instrucción perdida'; Notify:True),
      (Address:$14FA; Desc:'O Flujo inválido'; Notify:True),
      (Address:$1508; Desc:'P FN sin DEF FN'; Notify:True),
      (Address:$1516; Desc:'Q Error de parámetro'; Notify:True),
      (Address:$1525; Desc:'R Error de carga'; Notify:True),
      (Address:$1392; Desc:'a Error de MERGE'; Notify:True),
      (Address:$1392; Desc:'b Tipo de archivo incorrecto'; Notify:True),
      (Address:$1392; Desc:'c Error de CODE'; Notify:True),
      (Address:$1392; Desc:'d Demasiados paréntesis'; Notify:True),
      (Address:$1392; Desc:'e El archivo ya existe'; Notify:True),
      (Address:$1392; Desc:'f Nombre inválido'; Notify:True),
      (Address:$1392; Desc:'g Error de RAMDisk'; Notify:True),
      (Address:$1392; Desc:'h Archivo no encontrado'; Notify:True),
      (Address:$1392; Desc:'i Dispositivo inválido'; Notify:True),
      (Address:$1392; Desc:'j Baudios inválidos'; Notify:True),
      (Address:$1392; Desc:'k Nombre de nota inválido'; Notify:True),
      (Address:$1392; Desc:'l Número demasiado grande'; Notify:True),
      (Address:$1392; Desc:'m Nota fuera de rango'; Notify:True),
      (Address:$1392; Desc:'n Fuera de rango'; Notify:True),
      (Address:$1392; Desc:'o Demasiadas notas ligadas'; Notify:True),
      (Address:$1392; Desc:'? Error desconocido'; Notify:True));


implementation

Uses BASINMain;

procedure SetErrors(var lang: string);
var
  i, j: Integer;
Begin
  if (lang='Türkçe') then
  Begin
    // Errors dizisini kopyala
    for i := 0 to 43 do
      for j := 1 to 4 do
        Errors[i, j] := ErrorsTR[i, j];

    // ErrorAddresses dizisinin sadece Desc kýsmýný kopyala
    for i := 0 to 43 do
      ErrorAddresses[i].Desc := ErrorAddressesTR[i].Desc;

    exit;
  End;

  if (lang='English') then
  Begin
      for i := 0 to 43 do
      for j := 1 to 4 do
        Errors[i, j] := ErrorsEN[i, j];

           for i := 0 to 43 do
      ErrorAddresses[i].Desc := ErrorAddressesEN[i].Desc;
    exit;
  End;

  if (lang='Spanish') then
  Begin
    // Ýspanyolca Errors (Varsayýlan: ErrorsES)
    for i := 0 to 43 do
      for j := 1 to 4 do
        Errors[i, j] := ErrorsES[i, j];

    // Ýspanyolca ErrorAddresses (Varsayýlan: ErrorAddressesES)
    for i := 0 to 43 do
      ErrorAddresses[i].Desc := ErrorAddressesES[i].Desc;
      
    exit;
  end;

  if (lang='Deutsch') then
  Begin
    // Almanca Errors (Varsayýlan: ErrorsDE)
    for i := 0 to 43 do
      for j := 1 to 4 do
        Errors[i, j] := ErrorsDE[i, j];

    // Almanca ErrorAddresses (Varsayýlan: ErrorAddressesDE)
    for i := 0 to 43 do
      ErrorAddresses[i].Desc := ErrorAddressesDE[i].Desc;

    exit;
  end;
End;

Procedure SetLanguage(Var lang: String);



begin

if (lang='Türkçe') then
        Begin
        AnimPreviewWindow.Caption :='Canlandýrma Ön Ýzlemesi';
        AnimPreviewWindow.Label22.Caption :='Güncelleme Aralýðý:';
        AnimPreviewWindow.Label21.Caption :='xx kare';
        AnimPreviewWindow.Label2.Caption :='Büyüklük:';
        AnimPreviewWindow.Label1.Caption :='xx kare';
        AnimPreviewWindow.Panel1.Caption :='Panel1';
        AnimPreviewWindow.Button1.Caption :='Kapat';
        AnimPreviewWindow.Button2.Caption :='Kaydet...';
        AnimPreviewWindow.CheckBox1.Caption :='ileri-geri';
        
        
          // urceR14.20110529\BasinMain.dfm
        BASinOutput.Caption :=ReleaseName;
        //BASinOutput.Label1.Caption :='';
        BASinOutput.File1.Caption :='&Dosya';
        BASinOutput.New1.Caption :='Yeni';
        BASinOutput.N5.Caption :='-';
        BASinOutput.Load1.Caption :='Aç...';
        BASinOutput.ReLOAD1.Caption :='Son Açýlanlar';
        BASinOutput.PreviousSession1.Caption :='Önceki Oturumu Aç';
        BASinOutput.N12.Caption :='-';
        BASinOutput.Item11.Caption :='Item1';
        BASinOutput.Item21.Caption :='Item2';
        BASinOutput.Item31.Caption :='Item3';
        BASinOutput.Item41.Caption :='Item4';
        BASinOutput.Item51.Caption :='Item5';
        BASinOutput.Item61.Caption :='Item6';
        BASinOutput.Item71.Caption :='Item7';
        BASinOutput.Item81.Caption :='Item8';
        BASinOutput.ImportBASIC1.Caption :='Binary Aç...';
        BASinOutput.ImportfromTapeImage1.Caption :='Teyp Ýmajý Tak...';
        BASinOutput.N1.Caption :='-';
        BASinOutput.Save1.Caption :='Kaydet';
        BASinOutput.SaveBASICas1.Caption :='Farklý Kaydet...';
        BASinOutput.N2.Caption :='-';
        BASinOutput.Print1.Caption :='Yazdýr...';
        BASinOutput.N9.Caption :='-';
        BASinOutput.Exit1.Caption :='Çýk';
        BASinOutput.Edit1.Caption :='Düzen';
        BASinOutput.Undo1.Caption :='Geri Al';
        BASinOutput.Redo1.Caption :='Yinele';
        BASinOutput.N6.Caption :='-';
        BASinOutput.Cut1.Caption :='Kes';
        BASinOutput.Copy1.Caption :='Özel Kopyala';
        BASinOutput.Paste1.Caption :='Yapýþtýr';
        BASinOutput.Delete1.Caption :='Sil';
        BASinOutput.N14.Caption :='-';
        BASinOutput.CopyListing1.Caption :='Liste Olarak Kopyala';
        BASinOutput.View1.Caption :='&Görünüm';
        BASinOutput.ToolBar1.Caption :='Araç Çubuðu';
        BASinOutput.StatusBar2.Caption :='Durum Çubuðu';
        BASinOutput.SyntaxHelper1.Caption :='Sözdizim Yardýmcýsý';
        BASinOutput.CharacterRuler1.Caption :='Karakter Cetveli';
        BASinOutput.N15.Caption :='-';
        BASinOutput.ProgramInformation1.Caption :='Program Bilgisi';
        BASinOutput.ZXPrinterOutput1.Caption :='ZX Yazýcý Çýktýsý';
        BASinOutput.CommandHistory1.Caption :='Komut Geçmiþi';
        BASinOutput.LastError1.Caption :='Son Hata';
        BASinOutput.DebugWindows1.Caption :='Hata Ayýklama Pencereleri';
        BASinOutput.Variables1.Caption :='Deðiþkenler';
        BASinOutput.SystemVariables1.Caption :='Sistem Deðiþkenleri';
        BASinOutput.Breakpoints1.Caption :='Kesme Noktalarý';
        BASinOutput.Watches1.Caption :='Ýzlemeler';
        BASinOutput.GOSUBStack1.Caption :='GO SUB Yýðýný';
        BASinOutput.MemoryMap1.Caption :='Bellek Haritasý';
        BASinOutput.MemoryViewer1.Caption :='Bellek Listeleyici';
        BASinOutput.LogWindow1.Caption :='Kütük Penceresi';
        BASinOutput.ProfileResults1.Caption :='Performans Penceresi';
        BASinOutput.CPUWindow1.Caption :='CPU Penceresi';
        BASinOutput.WindowSize1.Caption :='Sanal Ekran Penceresi';
        BASinOutput.DisplayWindow1.Caption :='Görünür';
        BASinOutput.N100320x2401.Caption :='100% (320x240)';
        BASinOutput.N200640x4801.Caption :='200% (640x480)';
        BASinOutput.Custom1.Caption :='Özel ()';
        BASinOutput.N13.Caption :='-';
        BASinOutput.Force11Aspect1.Caption :='1:1 Oraný Zorla';
        BASinOutput.ExpressionEvaluator1.Caption :='Fonksiyon Hesaplama';
        BASinOutput.Search1.Caption :='Ara';
        BASinOutput.Find1.Caption :='Bul...';
        BASinOutput.Replace1.Caption :='Deðiþtir...';
        BASinOutput.FindNext1.Caption :='Sonrakini Bul';
        BASinOutput.ReplaceNext1.Caption :='Sonrakini Deðiþtir';
        BASinOutput.N10.Caption :='-';
        BASinOutput.SourceMarkers1.Caption :='Kaynak Ýmleri';
        BASinOutput.SetMarker1.Caption :='Ýmle';
        BASinOutput.Marker01.Caption :='Marker 0';
        BASinOutput.Marker11.Caption :='Marker 1';
        BASinOutput.Marker21.Caption :='Marker 2';
        BASinOutput.Marker31.Caption :='Marker 3';
        BASinOutput.Marker41.Caption :='Marker 4';
        BASinOutput.Marker51.Caption :='Marker 5';
        BASinOutput.Marker61.Caption :='Marker 6';
        BASinOutput.Marker71.Caption :='Marker 7';
        BASinOutput.Marker81.Caption :='Marker 8';
        BASinOutput.Marker91.Caption :='Marker 9';
        BASinOutput.GetMarker1.Caption :='Ýme Git';
        BASinOutput.Marker02.Caption :='Marker 0';
        BASinOutput.Marker12.Caption :='Marker 1';
        BASinOutput.Marker22.Caption :='Marker 2';
        BASinOutput.Marker32.Caption :='Marker 3';
        BASinOutput.Marker42.Caption :='Marker 4';
        BASinOutput.Marker52.Caption :='Marker 5';
        BASinOutput.Marker62.Caption :='Marker 6';
        BASinOutput.Marker72.Caption :='Marker 7';
        BASinOutput.Marker82.Caption :='Marker 8';
        BASinOutput.Marker92.Caption :='Marker 9';
        BASinOutput.N18.Caption :='-';
        BASinOutput.Clearall1.Caption :='Tümünü Kaldýr';
        BASinOutput.GotoLineNumber1.Caption :='Satýr numarasýna git...';
        BASinOutput.GotoError1.Caption :='Hataya git';
        BASinOutput.Run1.Caption :='Baþlat';
        BASinOutput.Run2.Caption :='Baþlat';
        BASinOutput.Continue1.Caption :='Devam et';
        BASinOutput.GOTO1.Caption :='Ýmleçe git';
        BASinOutput.EnableProfiling1.Caption :='Performans takibine baþla';
        BASinOutput.N11.Caption :='-';
        BASinOutput.ForceBREAK1.Caption :='Programý zorla durdur';
        BASinOutput.N7.Caption :='-';
        BASinOutput.TraceExecution1.Caption :='Ýþlem takibi';
        BASinOutput.SingleStepStatement1.Caption :='Tek basamak ilerle';
        BASinOutput.StepToNext1.Caption :='Sonraki basamaða atla';
        BASinOutput.RunTo1.Caption :='Ýmleçe kadar çalýþ';
        BASinOutput.N8.Caption :='-';
        BASinOutput.oggleBreakpoint1.Caption :='Kesme noktasý ekle/kaldýr';
        BASinOutput.AddBreakpoint1.Caption :='Kesme noktasý ekle...';
        BASinOutput.AddWatch1.Caption :='Takip ekle...';
        BASinOutput.N19.Caption :='-';
        BASinOutput.FullSpeed1.Caption :='Tam Hýz Emülasyon';
        BASinOutput.Tools1.Caption :='Araçlar';
        BASinOutput.BASinOptions1.Caption :='BasinC Seçenekleri...';
        BASinOutput.N4.Caption :='-';
        BASinOutput.TokenTable1.Caption :='Token Tablosu...';
        BASinOutput.BEEPComposer1.Caption :='BEEP Bestecisi...';
        BASinOutput.Memorygrabber1.Caption :='Bellek Yakalayýcý...';
        BASinOutput.UDGEditor1.Caption :='Grafik Editörü...';
        BASinOutput.ScreenPaintbox1.Caption :='Ekran Tuvali...';
        BASinOutput.Renumber1.Caption :='Yeniden Numaralandýr...';
        BASinOutput.TapeCreator1.Caption :='Teyp Kasedi Düzenleyici...';
        BASinOutput.Compiler1.Caption :='Derleyici';
        BASinOutput.Assembler1.Caption :='Assembler';
        BASinOutput.MemoryEditor1.Caption :='Bellek Düzenleyici';
        BASinOutput.Help1.Caption :='Yardým';
        BASinOutput.Contents1.Caption :='Ýçerik...';
        BASinOutput.CommandHelp1.Caption :='Komut Yardýmý';
        BASinOutput.SinclairBASICManual1.Caption :='Sinclair BASIC Kullanma Kýlavuzu';
        BASinOutput.ErrorHelp1.Caption :='Hata Yardýmý...';
        BASinOutput.N3.Caption :='-';
        BASinOutput.About1.Caption :='Hakkýnda...';
        BASinOutput.Token1.Caption :='Token';
        BASinOutput.Help2.Caption :='Token Yardýmý...';
        BASinOutput.Tokenise1.Caption :='Token yap';
        BASinOutput.EditVariable1.Caption :='Deðiþken Düzenle...';
        BASinOutput.FindLine1.Caption :='Satýrý Bul';
        BASinOutput.StringOperation1.Caption :='Dizi Operasyonu';
        BASinOutput.Wordwrapstring1.Caption :='Satýr sonu kelime aktar';
        BASinOutput.Splitat32chars1.Caption :='32 Karakterden kes';
        BASinOutput.Insertspaces1.Caption :='Boþluk ekle';
        BASinOutput.Tokeniseall1.Caption :='Tümünü token yap';
        BASinOutput.Detokeniseall1.Caption :='Tümünü metin yap';
        BASinOutput.N16.Caption :='-';
        BASinOutput.Cut2.Caption :='Kes';
        BASinOutput.Copy2.Caption :='Kopyala';
        BASinOutput.Paste2.Caption :='Yapýþtýr';
        BASinOutput.N17.Caption :='-';
        BASinOutput.Debug1.Caption :='Debug';
        BASinOutput.ToggleBreakpoint1.Caption :='Kesme noktasý ekle/kaldýr';
        BASinOutput.RunToCursor1.Caption :='Satýra kadar çalýþtýr';
        BASinOutput.GoToCursor1.Caption :='Satýra git';
        BASinOutput.WatchVariable1.Caption :='Deðiþkeni izle';
        // Missing BasinMain captions (from BasinMain.dfm 1.83.142) 
        BASinOutput.Label2.Caption :='Alt programa git';
        BASinOutput.LoadBasinState1.Caption :='Çalýþma alanýný aç...';
        BASinOutput.ExportTap1.Caption :='Tap aktar...';
        BASinOutput.SendtoExternalUtiility1.Caption :='SNA'#39'yý harici araca gönder...';
        BASinOutput.SaveWorkspace1.Caption :='Çalýþma alanýný kaydet...';
        BASinOutput.CopyAsPlainText2.Caption :='Düz metin olarak kopyala';
        BASinOutput.SaveListingasImage1.Caption :='Listeyi resim olarak kaydet';
        BASinOutput.SubRoutineList1.Caption :='Alt program listesi';
        BASinOutput.EnableSubList1.Caption :='Alt program listesini etkinleþtir';
        BASinOutput.N24.Caption :='-';
        BASinOutput.AutoDetectGoSubs1.Caption :='GO SUB'#39'larý otomatik algýla';
        BASinOutput.AutoDetectGOTOs2.Caption :='GO TO'#39'larý otomatik algýla';
        BASinOutput.PreviewOrigins1.Caption :='Atlama kaynaklarýný göster';
        BASinOutput.InfoLine1.Caption :='Bilgi satýrý';
        BASinOutput.CodeControlIcons1.Caption :='Kesme noktasý kontrol simgeleri';
        BASinOutput.SmartIndenting1.Caption :='Akýllý girintileme';
        BASinOutput.SnaptoEditor1.Caption :='Düzenleyici penceresine iliþtir';
        BASinOutput.AlwaysOnTop1.Caption :='Her zaman üstte';
        BASinOutput.PrintOutput1.Caption :='Ekran metni yakalama';
        BASinOutput.Notes1.Caption :='Yarýþma boyutunu göster';
        BASinOutput.AIChatPanel1.Caption :='Yapay zeka sohbet paneli';
        BASinOutput.BinaryImportExport1.Caption :='Ýkili iþe/dýþa aktarým';
        BASinOutput.OptimizeBasic1.Caption :='Basic temizle/küçült';
        BASinOutput.N20.Caption :='-';
        BASinOutput.simplecon1.Caption :='SimpleCon arayüzü';
        BASinOutput.N22.Caption :='-';
        BASinOutput.Editors1.Caption :='Editörler';
        BASinOutput.GraphEditor1.Caption :='Cheq Grafik Editörü';
        BASinOutput.SnippetEditor1.Caption :='Kod parçasý editörü';
        BASinOutput.N27.Caption :='-';
        BASinOutput.ProjectNotesEditor1.Caption :='Proje notlarý editörü';
        BASinOutput.UlaPlusPaletteEditor1.Caption :='UlaPlus palet editörü';
        BASinOutput.ShowaTip1.Caption :='Ýpucu göster';
        BASinOutput.BasinCOfficialDiscordServer1.Caption :='BasinC Resmi Discord Sunucusu';
        BASinOutput.BasinCShare1.Caption :='BasinC Paylaþ...';
        BASinOutput.CheqEditPage1.Caption :='CheqEdit Grafik Editörü Al';
        BASinOutput.GetPasmo1.Caption :='Pasmo-0.5.3 Windows Al';
        BASinOutput.N23.Caption :='-';
        BASinOutput.CheckUpdates1.Caption :='Güncellemeleri kontrol et';
        BASinOutput.N25.Caption :='-';
        BASinOutput.JumpTargets1.Caption :='Buraya atlamalarý izle';
        BASinOutput.EnableAutoSub1.Caption :='(Otomatik Sub Algýlama Kapalý)';
        BASinOutput.CopyAsPlainText1.Caption :='Kopyala';
        BASinOutput.SourceMarkers2.Caption :='Kaynak imleri';
        BASinOutput.SetMarker2.Caption :='Ým koy';
        BASinOutput.Marker04.Caption :='Marker 0';
        BASinOutput.Marker14.Caption :='Marker 1';
        BASinOutput.Marker24.Caption :='Marker 2';
        BASinOutput.Marker34.Caption :='Marker 3';
        BASinOutput.Marker44.Caption :='Marker 4';
        BASinOutput.Marker54.Caption :='Marker 5';
        BASinOutput.Marker64.Caption :='Marker 6';
        BASinOutput.Marker74.Caption :='Marker 7';
        BASinOutput.Marker84.Caption :='Marker 8';
        BASinOutput.Marker94.Caption :='Marker 9';
        BASinOutput.GetMarker2.Caption :='Ýme git';
        BASinOutput.Marker03.Caption :='Marker 0';
        BASinOutput.Marker13.Caption :='Marker 1';
        BASinOutput.Marker23.Caption :='Marker 2';
        BASinOutput.Marker33.Caption :='Marker 3';
        BASinOutput.Marker43.Caption :='Marker 4';
        BASinOutput.Marker53.Caption :='Marker 5';
        BASinOutput.Marker63.Caption :='Marker 6';
        BASinOutput.Marker73.Caption :='Marker 7';
        BASinOutput.Marker83.Caption :='Marker 8';
        BASinOutput.Marker93.Caption :='Marker 9';
        BASinOutput.N26.Caption :='-';
        BASinOutput.Clearall2.Caption :='Tümünü kaldýr';
        BASinOutput.AddNote1.Caption :='Not ekle';
        BASinOutput.AddSnippet1.Caption :='Kod parçalarýna ekle';
        BASinOutput.Share1.Caption :='Paylaþ...';
        // Missing BasinMain hints (from BasinMain.dfm)
        BASinOutput.SpeedButton1.Hint :='Yeni bir program oluþtur|';
        BASinOutput.SpeedButton2.Hint :='Bir program yükle|';
        BASinOutput.SpeedButton3.Hint :='Geçerli programý kaydet|';
        BASinOutput.SpeedButton5.Hint :='Ýmleçten baþlat|';
        BASinOutput.SpeedButton11.Hint :='Ýzleme ekle|';
        BASinOutput.SpeedButton12.Hint :='Deðerlendir|';
        BASinOutput.SpeedButton10.Hint :='Ýmleçte kesme noktasý ekle|';
        BASinOutput.SpeedButton8.Hint :='Üzerinden geç|';
        BASinOutput.SpeedButton7.Hint :='Tek adým|';
        BASinOutput.SpeedButton9.Hint :='Ýmlece kadar çalýþtýr|';
        BASinOutput.New1.Hint :='Yeni bir program oluþtur';
        BASinOutput.Load1.Hint :='Bir program yükle';
        BASinOutput.ReLOAD1.Hint :='Son yüklenen programlarýn listesi';
        BASinOutput.PreviousSession1.Hint :='BASinC'#39'i son oturum ayarlarýna geri yükle.';
        BASinOutput.ImportBASIC1.Hint :='Herhangi bir dosyadan BASIC veya CODE verisini oku';
        BASinOutput.ImportfromTapeImage1.Hint :='Sanal teyp imajý tak ve ondan yükle';
        BASinOutput.Save1.Hint :='Geçerli programý kaydet';
        BASinOutput.SaveBASICas1.Hint :='Geçerli programý bellekten çýkar';
        BASinOutput.Print1.Hint :='Geçerli programý yazdýr';
        BASinOutput.Exit1.Hint :='BASinC'#39'den çýk';
        BASinOutput.Undo1.Hint :='Son program deðiþikliðini geri al';
        BASinOutput.Redo1.Hint :='Geri alma sonrasý son deðiþiklikleri yinele';
        BASinOutput.Cut1.Hint :='Seçili metni kaldýr ve panoya gönder';
        BASinOutput.Copy1.Hint :='Seçili metni panoya kopyala';
        BASinOutput.CopyAsPlainText2.Hint :='Seçili metni panoya kopyala';
        BASinOutput.Paste1.Hint :='Panodaki metni düzenleme satýrýna yapýþtýr';
        BASinOutput.Delete1.Hint :='Seçili metni düzenleme satýrýndan kaldýr';
        BASinOutput.CopyListing1.Hint :='Tüm programý panoya kopyala';
        BASinOutput.DebugWindows1.Hint :='Hata ayýklama için kullanýlan pencereler';
        BASinOutput.Variables1.Hint :='Þu anda tanýmlý deðiþkenleri göster';
        BASinOutput.SystemVariables1.Hint :='Geçerli Sistem Deðiþkenlerini göster';
        BASinOutput.Breakpoints1.Hint :='Kullanýmdaki kesme noktalarýný göster';
        BASinOutput.Watches1.Hint :='Kullanýmdaki izleme koþullarýný listele';
        BASinOutput.GOSUBStack1.Hint :='GO SUB yýðýnýný göster';
        BASinOutput.MemoryMap1.Hint :='Bellek kullanýmýnýn görsel gösterimini göster.';
        BASinOutput.MemoryViewer1.Hint :='Spectrum'#39'un belleðini görüntüle veya düzenle';
        BASinOutput.LogWindow1.Hint :='Kaydedilen hata ayýklama çýktýsýný göster.';
        BASinOutput.ProfileResults1.Hint :='Son profil çalýþma sonuçlarýný göster';
        BASinOutput.CPUWindow1.Hint :='Assembly kodu için CPU penceresini göster';
        BASinOutput.SubRoutineList1.Hint :='REM #regionname ile tanýmlanan yerlerin listesini göster';
        BASinOutput.SyntaxHelper1.Hint :='BASIC sözdizimi denetleyicisini göster';
        BASinOutput.CharacterRuler1.Hint :='Dizge uzunluklarýný gösteren küçük bir "cetvel" göster';
        BASinOutput.CodeControlIcons1.Hint :='Araç çubuðunda kesme noktasý kontrol düðmelerini göster';
        BASinOutput.WindowSize1.Hint :='BASin boyutunu deðiþtir';
        BASinOutput.DisplayWindow1.Hint :='Çalýþma zamaný çýktý penceresini göster/gizle';
        BASinOutput.SnaptoEditor1.Hint :='Görüntü penceresini düzenleyici penceresine yapýþtýr';
        BASinOutput.N100320x2401.Hint :='Normal (1:1) pencere boyutu';
        BASinOutput.N200640x4801.Hint :='Çift (2:1) pencere boyutu';
        BASinOutput.Custom1.Hint :='Geçerli boyutlar';
        BASinOutput.Force11Aspect1.Hint :='BASinC'#39'in doðru en/boy oranýný korumasýný zorla';
        BASinOutput.ExpressionEvaluator1.Hint :='Hesap makinesi penceresini göster';
        BASinOutput.ProgramInformation1.Hint :='Geçerli program hakkýnda bilgi göster';
        BASinOutput.CommandHistory1.Hint :='Düzenleme satýrý geçmiþini listele';
        BASinOutput.LastError1.Hint :='Son çalýþma zamaný hata mesajýný göster';
        BASinOutput.Find1.Hint :='Geçerli program içinde metin bul';
        BASinOutput.Replace1.Hint :='Geçerli program içinde metni deðiþtir';
        BASinOutput.FindNext1.Hint :='Son Bul iþlemini yinele';
        BASinOutput.ReplaceNext1.Hint :='Son Deðiþtir iþlemini yinele';
        BASinOutput.SourceMarkers1.Hint :='Kaynak imleriyle çalýþ';
        BASinOutput.SetMarker1.Hint :='Kaynak imi koy';
        BASinOutput.Marker01.Hint :='Kaynak imi 0'#39'ý koy';
        BASinOutput.Marker11.Hint :='Kaynak imi 1'#39'i koy';
        BASinOutput.Marker21.Hint :='Kaynak imi 2'#39'yi koy';
        BASinOutput.Marker31.Hint :='Kaynak imi 3'#39'ü koy';
        BASinOutput.Marker41.Hint :='Kaynak imi 4'#39'ü koy';
        BASinOutput.Marker51.Hint :='Kaynak imi 5'#39'i koy';
        BASinOutput.Marker61.Hint :='Kaynak imi 6'#39'yý koy';
        BASinOutput.Marker71.Hint :='Kaynak imi 7'#39'yi koy';
        BASinOutput.Marker81.Hint :='Kaynak imi 8'#39'i koy';
        BASinOutput.Marker91.Hint :='Kaynak imi 9'#39'u koy';
        BASinOutput.GetMarker1.Hint :='Bir kaynak imine git';
        BASinOutput.Marker02.Hint :='Kaynak imi 0'#39'a git';
        BASinOutput.Marker12.Hint :='Kaynak imi 1'#39'e git';
        BASinOutput.Marker22.Hint :='Kaynak imi 2'#39'ye git';
        BASinOutput.Marker32.Hint :='Kaynak imi 3'#39'e git';
        BASinOutput.Marker42.Hint :='Kaynak imi 4'#39'e git';
        BASinOutput.Marker52.Hint :='Kaynak imi 5'#39'e git';
        BASinOutput.Marker62.Hint :='Kaynak imi 6'#39'ya git';
        BASinOutput.Marker72.Hint :='Kaynak imi 7'#39'ye git';
        BASinOutput.Marker82.Hint :='Kaynak imi 8'#39'e git';
        BASinOutput.Marker92.Hint :='Kaynak imi 9'#39'a git';
        BASinOutput.Clearall1.Hint :='Tüm kaynak imlerini temizle';
        BASinOutput.GotoLineNumber1.Hint :='Program imlecini bir satýr numarasýna ayarla';
        BASinOutput.GotoError1.Hint :='Son hata ifadesini bul';
        BASinOutput.Run2.Hint :='Programý baþlat veya durdur';
        BASinOutput.GOTO1.Hint :='Programý belirtilen satýrdan çalýþtýr';
        BASinOutput.EnableProfiling1.Hint :='Kodu zamanlamak için profil sistemini etkinleþtir';
        BASinOutput.ForceBREAK1.Hint :='Sistemi editöre geri döndürür.';
        BASinOutput.TraceExecution1.Hint :='Çalýþma anýnda program yürütmeyi editörde görsel göster';
        BASinOutput.SingleStepStatement1.Hint :='Programda adým adým ilerle';
        BASinOutput.StepToNext1.Hint :='Sonraki satýra ulaþana kadar ifadeleri çalýþtýr';
        BASinOutput.RunTo1.Hint :='Belirtilen satýra ulaþana kadar satýrlarý çalýþtýr';
        BASinOutput.oggleBreakpoint1.Hint :='Geçerli satýrda kesme noktasýný aç/kapat';
        BASinOutput.AddBreakpoint1.Hint :='Kesme noktasý listesine bir nokta ekle';
        BASinOutput.AddWatch1.Hint :='Ýzleme listesine bir koþul ekle';
        BASinOutput.BASinOptions1.Hint :='BASinC'#39'i yapýlandýr';
        BASinOutput.TokenTable1.Hint :='Tek týklamayla token giriþine izin verir';
        BASinOutput.Renumber1.Hint :='Geçerli programý yeniden numaralandýr';
        BASinOutput.ZXPrinterOutput1.Hint :='Öykünülen ZX yazýcý çýktýsýný göster';
        BASinOutput.Assembler1.Hint :='Tümleþik Z80 assembler'#39'ý aç';
        BASinOutput.Compiler1.Hint :='Geçerli programý asm metnine derle';
        BASinOutput.UDGEditor1.Hint :='UDG'#39'leri/karakterleri grafik olarak düzenle';
        BASinOutput.ScreenPaintbox1.Hint :='Ekran çizim aracý';
        BASinOutput.TapeCreator1.Hint :='Teyp (.tap/.tzx/.wav) dosyasý oluþtur';
        BASinOutput.BEEPComposer1.Hint :='BEEP ezgilerini görsel olarak oluþtur';
        BASinOutput.Contents1.Hint :='BASin yardým dosyasýný göster';
        BASinOutput.CommandHelp1.Hint :='Geçerli komut hakkýnda yardým al';
        BASinOutput.SinclairBASICManual1.Hint :='Sinclair BASIC kýlavuzunu göster';
        BASinOutput.ErrorHelp1.Hint :='Çalýþma zamaný hatalarý hakkýnda yardým al';
        BASinOutput.About1.Hint :='BASin hakkýnda';
        BASinOutput.SourceMarkers2.Hint :='Kaynak imleriyle çalýþ';
        BASinOutput.SetMarker2.Hint :='Kaynak imi koy';
        BASinOutput.Marker04.Hint :='Kaynak imi 0'#39'ý koy';
        BASinOutput.Marker14.Hint :='Kaynak imi 1'#39'i koy';
        BASinOutput.Marker24.Hint :='Kaynak imi 2'#39'yi koy';
        BASinOutput.Marker34.Hint :='Kaynak imi 3'#39'ü koy';
        BASinOutput.Marker44.Hint :='Kaynak imi 4'#39'ü koy';
        BASinOutput.Marker54.Hint :='Kaynak imi 5'#39'i koy';
        BASinOutput.Marker64.Hint :='Kaynak imi 6'#39'yý koy';
        BASinOutput.Marker74.Hint :='Kaynak imi 7'#39'yi koy';
        BASinOutput.Marker84.Hint :='Kaynak imi 8'#39'i koy';
        BASinOutput.Marker94.Hint :='Kaynak imi 9'#39'u koy';
        BASinOutput.GetMarker2.Hint :='Bir kaynak imine git';
        BASinOutput.Marker03.Hint :='Kaynak imi 0'#39'a git';
        BASinOutput.Marker13.Hint :='Kaynak imi 1'#39'e git';
        BASinOutput.Marker23.Hint :='Kaynak imi 2'#39'ye git';
        BASinOutput.Marker33.Hint :='Kaynak imi 3'#39'e git';
        BASinOutput.Marker43.Hint :='Kaynak imi 4'#39'e git';
        BASinOutput.Marker53.Hint :='Kaynak imi 5'#39'e git';
        BASinOutput.Marker63.Hint :='Kaynak imi 6'#39'ya git';
        BASinOutput.Marker73.Hint :='Kaynak imi 7'#39'ye git';
        BASinOutput.Marker83.Hint :='Kaynak imi 8'#39'e git';
        BASinOutput.Marker93.Hint :='Kaynak imi 9'#39'a git';
        BASinOutput.Clearall2.Hint :='Tüm kaynak imlerini temizle';

        

        end;

if (lang='English') then
        Begin
                  // urceR14.20110529\BasinMain.dfm
        BASinOutput.Caption :=ReleaseName;
        //BASinOutput.Label1.Caption :='Label1';
        BASinOutput.File1.Caption :='&File';
        BASinOutput.New1.Caption :='New';
        BASinOutput.N5.Caption :='-';
        BASinOutput.Load1.Caption :='Open...';
        BASinOutput.ReLOAD1.Caption :='Recent Files';
        BASinOutput.PreviousSession1.Caption :='Previous Session';
        BASinOutput.N12.Caption :='-';
        BASinOutput.Item11.Caption :='Item1';
        BASinOutput.Item21.Caption :='Item2';
        BASinOutput.Item31.Caption :='Item3';
        BASinOutput.Item41.Caption :='Item4';
        BASinOutput.Item51.Caption :='Item5';
        BASinOutput.Item61.Caption :='Item6';
        BASinOutput.Item71.Caption :='Item7';
        BASinOutput.Item81.Caption :='Item8';
        BASinOutput.ImportBASIC1.Caption :='Import Binary...';
        BASinOutput.ImportfromTapeImage1.Caption :='Attach Tape Image...';
        BASinOutput.N1.Caption :='-';
        BASinOutput.Save1.Caption :='Save';
        BASinOutput.SaveBASICas1.Caption :='Save As...';
        BASinOutput.N2.Caption :='-';
        BASinOutput.Print1.Caption :='Print...';
        BASinOutput.N9.Caption :='-';
        BASinOutput.Exit1.Caption :='Exit';
        BASinOutput.Edit1.Caption :='Edit';
        BASinOutput.Undo1.Caption :='Undo';
        BASinOutput.Redo1.Caption :='Redo';
        BASinOutput.N6.Caption :='-';
        BASinOutput.Cut1.Caption :='Cut';
        BASinOutput.Copy1.Caption :='Copy With Codes';
        BASinOutput.Paste1.Caption :='Paste';
        BASinOutput.Delete1.Caption :='Delete';
        BASinOutput.N14.Caption :='-';
        BASinOutput.CopyListing1.Caption :='Copy Listing';
        BASinOutput.View1.Caption :='&View';
        BASinOutput.ToolBar1.Caption :='Tool Bar';
        BASinOutput.StatusBar2.Caption :='Status Bar';
        BASinOutput.SyntaxHelper1.Caption :='Syntax Helper';
        BASinOutput.CharacterRuler1.Caption :='Character Ruler';
        BASinOutput.N15.Caption :='-';
        BASinOutput.ProgramInformation1.Caption :='Program Information';
        BASinOutput.ZXPrinterOutput1.Caption :='ZX Printer Output';
        BASinOutput.CommandHistory1.Caption :='Command History';
        BASinOutput.LastError1.Caption :='Last Error';
        BASinOutput.DebugWindows1.Caption :='Debug Windows';
        BASinOutput.Variables1.Caption :='Variables';
        BASinOutput.SystemVariables1.Caption :='System Variables';
        BASinOutput.Breakpoints1.Caption :='Breakpoints';
        BASinOutput.Watches1.Caption :='Watches';
        BASinOutput.GOSUBStack1.Caption :='GO SUB Stack';
        BASinOutput.MemoryMap1.Caption :='Memory Map';
        BASinOutput.MemoryViewer1.Caption :='Memory Viewer';
        BASinOutput.LogWindow1.Caption :='Log Window';
        BASinOutput.ProfileResults1.Caption :='Profile Results';
        BASinOutput.CPUWindow1.Caption :='CPU Window';
        BASinOutput.WindowSize1.Caption :='Display Window';
        BASinOutput.DisplayWindow1.Caption :='Visible';
        BASinOutput.N100320x2401.Caption :='100% (320x240)';
        BASinOutput.N200640x4801.Caption :='200% (640x480)';
        BASinOutput.Custom1.Caption :='Custom ()';
        BASinOutput.N13.Caption :='-';
        BASinOutput.Force11Aspect1.Caption :='Force 1:1 Aspect';
        BASinOutput.ExpressionEvaluator1.Caption :='Expression Evaluator';
        BASinOutput.Search1.Caption :='Search';
        BASinOutput.Find1.Caption :='Find...';
        BASinOutput.Replace1.Caption :='Replace...';
        BASinOutput.FindNext1.Caption :='Find Next';
        BASinOutput.ReplaceNext1.Caption :='Replace Next';
        BASinOutput.N10.Caption :='-';
        BASinOutput.SourceMarkers1.Caption :='Source Markers';
        BASinOutput.SetMarker1.Caption :='Set Marker';
        BASinOutput.Marker01.Caption :='Marker 0';
        BASinOutput.Marker11.Caption :='Marker 1';
        BASinOutput.Marker21.Caption :='Marker 2';
        BASinOutput.Marker31.Caption :='Marker 3';
        BASinOutput.Marker41.Caption :='Marker 4';
        BASinOutput.Marker51.Caption :='Marker 5';
        BASinOutput.Marker61.Caption :='Marker 6';
        BASinOutput.Marker71.Caption :='Marker 7';
        BASinOutput.Marker81.Caption :='Marker 8';
        BASinOutput.Marker91.Caption :='Marker 9';
        BASinOutput.GetMarker1.Caption :='Get Marker';
        BASinOutput.Marker02.Caption :='Marker 0';
        BASinOutput.Marker12.Caption :='Marker 1';
        BASinOutput.Marker22.Caption :='Marker 2';
        BASinOutput.Marker32.Caption :='Marker 3';
        BASinOutput.Marker42.Caption :='Marker 4';
        BASinOutput.Marker52.Caption :='Marker 5';
        BASinOutput.Marker62.Caption :='Marker 6';
        BASinOutput.Marker72.Caption :='Marker 7';
        BASinOutput.Marker82.Caption :='Marker 8';
        BASinOutput.Marker92.Caption :='Marker 9';
        BASinOutput.N18.Caption :='-';
        BASinOutput.Clearall1.Caption :='Clear all';
        BASinOutput.GotoLineNumber1.Caption :='Go to Line Number...';
        BASinOutput.GotoError1.Caption :='Go to Error';
        BASinOutput.Run1.Caption :='Run';
        BASinOutput.Run2.Caption :='Run';
        BASinOutput.Continue1.Caption :='Continue';
        BASinOutput.GOTO1.Caption :='Go to Cursor';
        BASinOutput.EnableProfiling1.Caption :='Enable Profiling';
        BASinOutput.N11.Caption :='-';
        BASinOutput.ForceBREAK1.Caption :='Force BREAK';
        BASinOutput.N7.Caption :='-';
        BASinOutput.TraceExecution1.Caption :='Trace Execution';
        BASinOutput.SingleStepStatement1.Caption :='Single Step Statement';
        BASinOutput.StepToNext1.Caption :='Step Over Statement';
        BASinOutput.RunTo1.Caption :='Run To Cursor';
        BASinOutput.N8.Caption :='-';
        BASinOutput.oggleBreakpoint1.Caption :='Toggle Breakpoint';
        BASinOutput.AddBreakpoint1.Caption :='Add Breakpoint...';
        BASinOutput.AddWatch1.Caption :='Add Watch...';
        BASinOutput.N19.Caption :='-';
        BASinOutput.FullSpeed1.Caption :='Full Speed Emulation';
        BASinOutput.Tools1.Caption :='Tools';
        BASinOutput.BASinOptions1.Caption :='BasinC Options...';
        BASinOutput.N4.Caption :='-';
        BASinOutput.TokenTable1.Caption :='Token Table';
        BASinOutput.BEEPComposer1.Caption :='BEEP Composer';
        BASinOutput.Memorygrabber1.Caption :='Memory grabber';
        BASinOutput.UDGEditor1.Caption :='UDG Character Editor';
        BASinOutput.ScreenPaintbox1.Caption :='Image Editor';
        BASinOutput.Renumber1.Caption :='Renumber';
        BASinOutput.TapeCreator1.Caption :='Tape Editor';
        BASinOutput.Compiler1.Caption :='Compiler';
        BASinOutput.Assembler1.Caption :='Assembler';
        BASinOutput.MemoryEditor1.Caption :='Memory Manager';
        BASinOutput.Help1.Caption :='Help';
        BASinOutput.Contents1.Caption :='Contents...';
        BASinOutput.CommandHelp1.Caption :='Command Help';
        BASinOutput.SinclairBASICManual1.Caption :='Sinclair BASIC Manual';
        BASinOutput.ErrorHelp1.Caption :='Error Help...';
        BASinOutput.N3.Caption :='-';
        BASinOutput.About1.Caption :='About...';
        BASinOutput.Token1.Caption :='Token';
        BASinOutput.Help2.Caption :='Token Help...';
        BASinOutput.Tokenise1.Caption :='Tokenise';
        BASinOutput.EditVariable1.Caption :='Edit Variable...';
        BASinOutput.FindLine1.Caption :='Find Line';
        BASinOutput.StringOperation1.Caption :='String Operation';
        BASinOutput.Wordwrapstring1.Caption :='Wordwrap';
        BASinOutput.Splitat32chars1.Caption :='Split at 32 chars';
        BASinOutput.Insertspaces1.Caption :='Insert spaces';
        BASinOutput.Tokeniseall1.Caption :='Tokenise all';
        BASinOutput.Detokeniseall1.Caption :='Detokenise all';
        BASinOutput.N16.Caption :='-';
        BASinOutput.Cut2.Caption :='Cut';
        BASinOutput.Copy2.Caption :='Copy';
        BASinOutput.Paste2.Caption :='Paste';
        BASinOutput.N17.Caption :='-';
        BASinOutput.Debug1.Caption :='Debug';
        BASinOutput.ToggleBreakpoint1.Caption :='Toggle Breakpoint';
        BASinOutput.RunToCursor1.Caption :='Run To Line';
        BASinOutput.GoToCursor1.Caption :='Go To Line';
        BASinOutput.WatchVariable1.Caption :='Watch Variable';
        // Missing BasinMain captions (from BasinMain.dfm)
        BASinOutput.Label2.Caption :='Go to sub';
        BASinOutput.LoadBasinState1.Caption :='Open Workspace...';
        BASinOutput.ExportTap1.Caption :='Export Tap...';
        BASinOutput.SendtoExternalUtiility1.Caption :='Send SNA to External Utility...';
        BASinOutput.SaveWorkspace1.Caption :='Save Workspace...';
        BASinOutput.CopyAsPlainText2.Caption :='Copy';
        BASinOutput.SaveListingasImage1.Caption :='Save Listing as Image';
        BASinOutput.SubRoutineList1.Caption :='Sub-Routine List';
        BASinOutput.EnableSubList1.Caption :='Enable Sub List';
        BASinOutput.N24.Caption :='-';
        BASinOutput.AutoDetectGoSubs1.Caption :='Auto Detect Go Sub'#39's';
        BASinOutput.AutoDetectGOTOs2.Caption :='Auto Detect GO TO'#39's';
        BASinOutput.PreviewOrigins1.Caption :='Show Jump Origins';
        BASinOutput.InfoLine1.Caption :='Info Line';
        BASinOutput.CodeControlIcons1.Caption :='Breakpoint Control Icons';
        BASinOutput.SmartIndenting1.Caption :='Smart Indenting';
        BASinOutput.SnaptoEditor1.Caption :='Attach to Editor Window';
        BASinOutput.AlwaysOnTop1.Caption :='Always On Top';
        BASinOutput.PrintOutput1.Caption :='Screen Text Capture';
        BASinOutput.Notes1.Caption :='Show Competition Size';
        BASinOutput.AIChatPanel1.Caption :='AI Chat Panel';
        BASinOutput.BinaryImportExport1.Caption :='Binary Import/Export';
        BASinOutput.OptimizeBasic1.Caption :='Clean/Minify Basic';
        BASinOutput.N20.Caption :='-';
        BASinOutput.simplecon1.Caption :='SimpleCon Interface';
        BASinOutput.N22.Caption :='-';
        BASinOutput.Editors1.Caption :='Editors';
        BASinOutput.GraphEditor1.Caption :='Cheq Graph Editor';
        BASinOutput.SnippetEditor1.Caption :='Snippet Editor';
        BASinOutput.N27.Caption :='-';
        BASinOutput.ProjectNotesEditor1.Caption :='Project Notes Editor';
        BASinOutput.UlaPlusPaletteEditor1.Caption :='UlaPlus Palette Editor';
        BASinOutput.ShowaTip1.Caption :='Show a Tip';
        BASinOutput.BasinCOfficialDiscordServer1.Caption :='BasinC Official Discord Server';
        BASinOutput.BasinCShare1.Caption :='BasinC Share...';
        BASinOutput.CheqEditPage1.Caption :='Get CheqEdit Graph Editor';
        BASinOutput.GetPasmo1.Caption :='Get Pasmo-0.5.3 Windows';
        BASinOutput.N23.Caption :='-';
        BASinOutput.CheckUpdates1.Caption :='Check Updates';
        BASinOutput.N25.Caption :='-';
        BASinOutput.JumpTargets1.Caption :='Retrace jumps to here';
        BASinOutput.EnableAutoSub1.Caption :='(Auto Sub Detection is Disabled)';
        BASinOutput.CopyAsPlainText1.Caption :='Copy';
        BASinOutput.SourceMarkers2.Caption :='Source Markers';
        BASinOutput.SetMarker2.Caption :='Set Marker';
        BASinOutput.Marker04.Caption :='Marker 0';
        BASinOutput.Marker14.Caption :='Marker 1';
        BASinOutput.Marker24.Caption :='Marker 2';
        BASinOutput.Marker34.Caption :='Marker 3';
        BASinOutput.Marker44.Caption :='Marker 4';
        BASinOutput.Marker54.Caption :='Marker 5';
        BASinOutput.Marker64.Caption :='Marker 6';
        BASinOutput.Marker74.Caption :='Marker 7';
        BASinOutput.Marker84.Caption :='Marker 8';
        BASinOutput.Marker94.Caption :='Marker 9';
        BASinOutput.GetMarker2.Caption :='Get Marker';
        BASinOutput.Marker03.Caption :='Marker 0';
        BASinOutput.Marker13.Caption :='Marker 1';
        BASinOutput.Marker23.Caption :='Marker 2';
        BASinOutput.Marker33.Caption :='Marker 3';
        BASinOutput.Marker43.Caption :='Marker 4';
        BASinOutput.Marker53.Caption :='Marker 5';
        BASinOutput.Marker63.Caption :='Marker 6';
        BASinOutput.Marker73.Caption :='Marker 7';
        BASinOutput.Marker83.Caption :='Marker 8';
        BASinOutput.Marker93.Caption :='Marker 9';
        BASinOutput.N26.Caption :='-';
        BASinOutput.Clearall2.Caption :='Clear all';
        BASinOutput.AddNote1.Caption :='Add Note';
        BASinOutput.AddSnippet1.Caption :='Add To Snippets';
        BASinOutput.Share1.Caption :='Share...';
        // Missing BasinMain hints (from BasinMain.dfm)
        BASinOutput.SpeedButton1.Hint :='Create a New program|';
        BASinOutput.SpeedButton2.Hint :='Load a Program|';
        BASinOutput.SpeedButton3.Hint :='Save the current program|';
        BASinOutput.SpeedButton5.Hint :='Start from Cursor|';
        BASinOutput.SpeedButton11.Hint :='Add Watch|';
        BASinOutput.SpeedButton12.Hint :='Evaluate|';
        BASinOutput.SpeedButton10.Hint :='Add Breakpoint at cursor|';
        BASinOutput.SpeedButton8.Hint :='Step Over|';
        BASinOutput.SpeedButton7.Hint :='Single Step|';
        BASinOutput.SpeedButton9.Hint :='Run To Cursor|';
        BASinOutput.New1.Hint :='Create a New program';
        BASinOutput.Load1.Hint :='Load a program';
        BASinOutput.ReLOAD1.Hint :='A list of recently loaded programs';
        BASinOutput.PreviousSession1.Hint :='Restore BASin to the last session'#39's setup.';
        BASinOutput.ImportBASIC1.Hint :='Read BASIC or CODE data from any file';
        BASinOutput.ImportfromTapeImage1.Hint :='Insert a virtual tape image and load from it';
        BASinOutput.Save1.Hint :='Save the current program';
        BASinOutput.SaveBASICas1.Hint :='Extract the current program from memory';
        BASinOutput.Print1.Hint :='Print the current program';
        BASinOutput.Exit1.Hint :='Quit BASin';
        BASinOutput.Undo1.Hint :='Undo the last program change';
        BASinOutput.Redo1.Hint :='Redo your last changes after an Undo';
        BASinOutput.Cut1.Hint :='Remove the selected text and send it to the clipboard';
        BASinOutput.Copy1.Hint :='Copy the selected text to the clipboard';
        BASinOutput.CopyAsPlainText2.Hint :='Copy the selected text to the clipboard';
        BASinOutput.Paste1.Hint :='Paste text stored on the clipboard to the edit line';
        BASinOutput.Delete1.Hint :='Remove the selected text from the edit line';
        BASinOutput.CopyListing1.Hint :='Copy the entire program to the Clipboard';
        BASinOutput.DebugWindows1.Hint :='Windows used for debugging';
        BASinOutput.Variables1.Hint :='Show the currently declared variables';
        BASinOutput.SystemVariables1.Hint :='Show the curent System Variables';
        BASinOutput.Breakpoints1.Hint :='View any breakpoints currently in use';
        BASinOutput.Watches1.Hint :='List any watch conditions in use';
        BASinOutput.GOSUBStack1.Hint :='Show the GO SUB stack';
        BASinOutput.MemoryMap1.Hint :='Shows a visual display of memory usage.';
        BASinOutput.MemoryViewer1.Hint :='View or Edit the Spectrum'#39's Memory';
        BASinOutput.LogWindow1.Hint :='View any debug output logged.';
        BASinOutput.ProfileResults1.Hint :='Show the results of the last code-profile operation';
        BASinOutput.CPUWindow1.Hint :='Show the CPU window for debugging assembly code';
        BASinOutput.SubRoutineList1.Hint :='Shows a list of places defined by REM #regionname';
        BASinOutput.SyntaxHelper1.Hint :='Show the BASIC syntax checker';
        BASinOutput.CharacterRuler1.Hint :='Shows a small "ruler" which depicts string lengths';
        BASinOutput.CodeControlIcons1.Hint :='Show breakpoint control buttons on toolbar';
        BASinOutput.WindowSize1.Hint :='Change the size of BASin';
        BASinOutput.DisplayWindow1.Hint :='Show or hide the runtime output window';
        BASinOutput.SnaptoEditor1.Hint :='Snap Display Window to Editor Window';
        BASinOutput.N100320x2401.Hint :='Normal (1:1) Window size';
        BASinOutput.N200640x4801.Hint :='Double (2:1) Window Size';
        BASinOutput.Custom1.Hint :='The current dimensions';
        BASinOutput.Force11Aspect1.Hint :='Force BASin to maintain correct width/height ratio';
        BASinOutput.ExpressionEvaluator1.Hint :='Show the calculator window';
        BASinOutput.ProgramInformation1.Hint :='View information about the current program';
        BASinOutput.CommandHistory1.Hint :='List the edit line history';
        BASinOutput.LastError1.Hint :='View the last runtime error message';
        BASinOutput.Find1.Hint :='Find text within the current program';
        BASinOutput.Replace1.Hint :='Replace text within the current program';
        BASinOutput.FindNext1.Hint :='Repeat the last Find operation';
        BASinOutput.ReplaceNext1.Hint :='Repeat the last Replace operation';
        BASinOutput.SourceMarkers1.Hint :='Work with Source Markers';
        BASinOutput.SetMarker1.Hint :='Set Source Markers';
        BASinOutput.Marker01.Hint :='Set Source Marker 0';
        BASinOutput.Marker11.Hint :='Set Source Marker 1';
        BASinOutput.Marker21.Hint :='Set Source Marker 2';
        BASinOutput.Marker31.Hint :='Set Source Marker 3';
        BASinOutput.Marker41.Hint :='Set Source Marker 4';
        BASinOutput.Marker51.Hint :='Set Source Marker 5';
        BASinOutput.Marker61.Hint :='Set Source Marker 6';
        BASinOutput.Marker71.Hint :='Set Source Marker 7';
        BASinOutput.Marker81.Hint :='Set Source Marker 8';
        BASinOutput.Marker91.Hint :='Set Source Marker 9';
        BASinOutput.GetMarker1.Hint :='Jump to a source marker';
        BASinOutput.Marker02.Hint :='Jump to Source Marker 0';
        BASinOutput.Marker12.Hint :='Jump to Source Marker 1';
        BASinOutput.Marker22.Hint :='Jump to Source Marker 2';
        BASinOutput.Marker32.Hint :='Jump to Source Marker 3';
        BASinOutput.Marker42.Hint :='Jump to Source Marker 4';
        BASinOutput.Marker52.Hint :='Jump to Source Marker 5';
        BASinOutput.Marker62.Hint :='Jump to Source Marker 6';
        BASinOutput.Marker72.Hint :='Jump to Source Marker 7';
        BASinOutput.Marker82.Hint :='Jump to Source Marker 8';
        BASinOutput.Marker92.Hint :='Jump to Source Marker 9';
        BASinOutput.Clearall1.Hint :='Clear all Source Markers';
        BASinOutput.GotoLineNumber1.Hint :='Set the program cursor to a line number';
        BASinOutput.GotoError1.Hint :='Find the last error statement';
        BASinOutput.Run2.Hint :='Start or Stop the program';
        BASinOutput.GOTO1.Hint :='Run the orogram from a specified line number';
        BASinOutput.EnableProfiling1.Hint :='Enable the profiling system to time your code';
        BASinOutput.ForceBREAK1.Hint :='Restores the system to the editor.';
        BASinOutput.TraceExecution1.Hint :='Visually show program execution in the editor at runtime';
        BASinOutput.SingleStepStatement1.Hint :='Step through the program';
        BASinOutput.StepToNext1.Hint :='Execute statements until the next line is reached';
        BASinOutput.RunTo1.Hint :='Execute lines until a specified line is reached';
        BASinOutput.oggleBreakpoint1.Hint :='Toggle a breakpoint at the current line';
        BASinOutput.AddBreakpoint1.Hint :='Add a breakpoint to the breakpoint list';
        BASinOutput.AddWatch1.Hint :='Add a watch condition to the Watch List';
        BASinOutput.BASinOptions1.Hint :='Configure BASin';
        BASinOutput.TokenTable1.Hint :='Allows single click Token entry';
        BASinOutput.Renumber1.Hint :='Renumber the current program';
        BASinOutput.ZXPrinterOutput1.Hint :='Show the output of the emulated ZX Printer';
        BASinOutput.Assembler1.Hint :='Open the integrated Z80 Assembler';
        BASinOutput.Compiler1.Hint :='Compile the current program to asm text';
        BASinOutput.UDGEditor1.Hint :='Graphically edit the UDGs/Characters';
        BASinOutput.ScreenPaintbox1.Hint :='Screen paint utility';
        BASinOutput.TapeCreator1.Hint :='Create a Tape (.tap/.tzx/.wav) file';
        BASinOutput.BEEPComposer1.Hint :='Create BEEP tunes visually';
        BASinOutput.Contents1.Hint :='Show the BASin help file';
        BASinOutput.CommandHelp1.Hint :='Get help on the current command';
        BASinOutput.SinclairBASICManual1.Hint :='Show the Sinclair BASIC manual';
        BASinOutput.ErrorHelp1.Hint :='Get help about runtime errors';
        BASinOutput.About1.Hint :='About BASin';
        BASinOutput.SourceMarkers2.Hint :='Work with Source Markers';
        BASinOutput.SetMarker2.Hint :='Set Source Markers';
        BASinOutput.Marker04.Hint :='Set Source Marker 0';
        BASinOutput.Marker14.Hint :='Set Source Marker 1';
        BASinOutput.Marker24.Hint :='Set Source Marker 2';
        BASinOutput.Marker34.Hint :='Set Source Marker 3';
        BASinOutput.Marker44.Hint :='Set Source Marker 4';
        BASinOutput.Marker54.Hint :='Set Source Marker 5';
        BASinOutput.Marker64.Hint :='Set Source Marker 6';
        BASinOutput.Marker74.Hint :='Set Source Marker 7';
        BASinOutput.Marker84.Hint :='Set Source Marker 8';
        BASinOutput.Marker94.Hint :='Set Source Marker 9';
        BASinOutput.GetMarker2.Hint :='Jump to a source marker';
        BASinOutput.Marker03.Hint :='Jump to Source Marker 0';
        BASinOutput.Marker13.Hint :='Jump to Source Marker 1';
        BASinOutput.Marker23.Hint :='Jump to Source Marker 2';
        BASinOutput.Marker33.Hint :='Jump to Source Marker 3';
        BASinOutput.Marker43.Hint :='Jump to Source Marker 4';
        BASinOutput.Marker53.Hint :='Jump to Source Marker 5';
        BASinOutput.Marker63.Hint :='Jump to Source Marker 6';
        BASinOutput.Marker73.Hint :='Jump to Source Marker 7';
        BASinOutput.Marker83.Hint :='Jump to Source Marker 8';
        BASinOutput.Marker93.Hint :='Jump to Source Marker 9';
        BASinOutput.Clearall2.Hint :='Clear all Source Markers';
        end;

if (lang='Deutsch') then
        Begin
                  // urceR14.20110529\BasinMain.dfm
        BASinOutput.Caption :=ReleaseName;
        //BASinOutput.Label1.Caption :='Label1';
        BASinOutput.File1.Caption :='&Datei';
        BASinOutput.New1.Caption :='&Neu';
        BASinOutput.N5.Caption :='-';
        BASinOutput.Load1.Caption :='Öffnen...';
        BASinOutput.ReLOAD1.Caption :='Wieder öffnen';
        BASinOutput.PreviousSession1.Caption :='Vorherige Session';
        BASinOutput.N12.Caption :='-';
        BASinOutput.Item11.Caption :='Item1';
        BASinOutput.Item21.Caption :='Item2';
        BASinOutput.Item31.Caption :='Item3';
        BASinOutput.Item41.Caption :='Item4';
        BASinOutput.Item51.Caption :='Item5';
        BASinOutput.Item61.Caption :='Item6';
        BASinOutput.Item71.Caption :='Item7';
        BASinOutput.Item81.Caption :='Item8';
        BASinOutput.ImportBASIC1.Caption :='Binärdatei importieren...';
        BASinOutput.ImportfromTapeImage1.Caption :='Band-Image anfügen...';
        BASinOutput.N1.Caption :='-';
        BASinOutput.Save1.Caption :='Speichern';
        BASinOutput.SaveBASICas1.Caption :='Speichern &unter...';
        BASinOutput.N2.Caption :='-';
        BASinOutput.Print1.Caption :='Drucken...';
        BASinOutput.N9.Caption :='-';
        BASinOutput.Exit1.Caption :='Beenden';
        BASinOutput.Edit1.Caption :='Edit';
        BASinOutput.Undo1.Caption :='Rückgängig';
        BASinOutput.Redo1.Caption :='Wiederholen';
        BASinOutput.N6.Caption :='-';
        BASinOutput.Cut1.Caption :='Ausschneiden';
        BASinOutput.Copy1.Caption :='Spezial Kopieren';
        BASinOutput.Paste1.Caption :='Einfügen';
        BASinOutput.Delete1.Caption :='Löschen';
        BASinOutput.N14.Caption :='-';
        BASinOutput.CopyListing1.Caption :='Listing kopieren';
        BASinOutput.View1.Caption :='Ansicht';
        BASinOutput.ToolBar1.Caption :='Symbolleiste';
        BASinOutput.StatusBar2.Caption :='Statusleiste';
        BASinOutput.SyntaxHelper1.Caption :='Syntax-Hilfe';
        BASinOutput.CharacterRuler1.Caption :='Zeichenlineal';
        BASinOutput.N15.Caption :='-';
        BASinOutput.ProgramInformation1.Caption :='Programminformation';
        BASinOutput.ZXPrinterOutput1.Caption :='ZX Druckerausgabe';
        BASinOutput.CommandHistory1.Caption :='Befehlsverlauf';
        BASinOutput.LastError1.Caption :='Letzter Fehler';
        BASinOutput.DebugWindows1.Caption :='Debug-Fenster';
        BASinOutput.Variables1.Caption :='Variablen';
        BASinOutput.SystemVariables1.Caption :='Systemvariablen';
        BASinOutput.Breakpoints1.Caption :='Haltepunkte';
        BASinOutput.Watches1.Caption :='Überwachung';
        BASinOutput.GOSUBStack1.Caption :='GO SUB Stack';
        BASinOutput.MemoryMap1.Caption :='Speicherkarte';
        BASinOutput.MemoryViewer1.Caption :='Speicheransicht';
        BASinOutput.LogWindow1.Caption :='Log-Fenster';
        BASinOutput.ProfileResults1.Caption :='Profile Results';
        BASinOutput.CPUWindow1.Caption :='CPU-Fenster';
        BASinOutput.WindowSize1.Caption :='Anzeigefenster';
        BASinOutput.DisplayWindow1.Caption :='sichtbar';
        BASinOutput.N100320x2401.Caption :='100% (320x240)';
        BASinOutput.N200640x4801.Caption :='200% (640x480)';
        BASinOutput.Custom1.Caption :='Benutzerdefiniert ()';
        BASinOutput.N13.Caption :='-';
        BASinOutput.Force11Aspect1.Caption :='1:1-Verhältnis';
        BASinOutput.ExpressionEvaluator1.Caption :='Ausdrücke auswerten';
        BASinOutput.Search1.Caption :='&Suchen';
        BASinOutput.Find1.Caption :='&Suchen...';
        BASinOutput.Replace1.Caption :='&Ersetzen...';
        BASinOutput.FindNext1.Caption :='Weiter Suchen';
        BASinOutput.ReplaceNext1.Caption :='Nächstes ersetzen';
        BASinOutput.N10.Caption :='-';
        BASinOutput.SourceMarkers1.Caption :='Quelltext-Marker';
        BASinOutput.SetMarker1.Caption :='Setze Marker';
        BASinOutput.Marker01.Caption :='Marker 0';
        BASinOutput.Marker11.Caption :='Marker 1';
        BASinOutput.Marker21.Caption :='Marker 2';
        BASinOutput.Marker31.Caption :='Marker 3';
        BASinOutput.Marker41.Caption :='Marker 4';
        BASinOutput.Marker51.Caption :='Marker 5';
        BASinOutput.Marker61.Caption :='Marker 6';
        BASinOutput.Marker71.Caption :='Marker 7';
        BASinOutput.Marker81.Caption :='Marker 8';
        BASinOutput.Marker91.Caption :='Marker 9';
        BASinOutput.GetMarker1.Caption :='Get Marker';
        BASinOutput.Marker02.Caption :='Marker 0';
        BASinOutput.Marker12.Caption :='Marker 1';
        BASinOutput.Marker22.Caption :='Marker 2';
        BASinOutput.Marker32.Caption :='Marker 3';
        BASinOutput.Marker42.Caption :='Marker 4';
        BASinOutput.Marker52.Caption :='Marker 5';
        BASinOutput.Marker62.Caption :='Marker 6';
        BASinOutput.Marker72.Caption :='Marker 7';
        BASinOutput.Marker82.Caption :='Marker 8';
        BASinOutput.Marker92.Caption :='Marker 9';
        BASinOutput.N18.Caption :='-';
        BASinOutput.Clearall1.Caption :='Alle löschen';
        BASinOutput.GotoLineNumber1.Caption :='Gehe zu Zeilennummer...';
        BASinOutput.GotoError1.Caption :='Gehe zu Fehler';
        BASinOutput.Run1.Caption :='Run';
        BASinOutput.Run2.Caption :='Run';
        BASinOutput.Continue1.Caption :='Fortsetzen';
        BASinOutput.GOTO1.Caption :='Gehe zum Cursor';
        BASinOutput.EnableProfiling1.Caption :='Profiling';
        BASinOutput.N11.Caption :='-';
        BASinOutput.ForceBREAK1.Caption :='BREAK erzwingen';
        BASinOutput.N7.Caption :='-';
        BASinOutput.TraceExecution1.Caption :='Trace Execution';
        BASinOutput.SingleStepStatement1.Caption :='Einzelschritt';
        BASinOutput.StepToNext1.Caption :='Step Over Statement';
        BASinOutput.RunTo1.Caption :='Laufe zum Cursor';
        BASinOutput.N8.Caption :='-';
        BASinOutput.oggleBreakpoint1.Caption :='Wechsele Haltepunkt';
        BASinOutput.AddBreakpoint1.Caption :='Haltepunkt hinzufügen...';
        BASinOutput.AddWatch1.Caption :='Watch hinzufügen...';
        BASinOutput.N19.Caption :='-';
        BASinOutput.FullSpeed1.Caption :='maximale Geschwindigkeit';
        BASinOutput.Tools1.Caption :='Werkzeuge';
        BASinOutput.BASinOptions1.Caption :='Einstellungen...';
        BASinOutput.N4.Caption :='-';
        BASinOutput.TokenTable1.Caption :='Token Tabelle';
        BASinOutput.BEEPComposer1.Caption :='BEEP Composer';
        BASinOutput.Memorygrabber1.Caption :='Memory grabber';
        BASinOutput.UDGEditor1.Caption :='Graphik/Sprite-Editor';
        BASinOutput.ScreenPaintbox1.Caption :='Screen Paintbox';
        BASinOutput.Renumber1.Caption :='Neu nummerieren';
        BASinOutput.TapeCreator1.Caption :='Tape Creator';
        BASinOutput.Compiler1.Caption :='Compiler';
        BASinOutput.Assembler1.Caption :='Assembler';
        BASinOutput.MemoryEditor1.Caption :='Memory Editor';
        BASinOutput.Help1.Caption :='Hilfe';
        BASinOutput.Contents1.Caption :='Inhalt...';
        BASinOutput.CommandHelp1.Caption :='Hilfe zu Befehlen';
        BASinOutput.SinclairBASICManual1.Caption :='Sinclair BASIC Handbuch';
        BASinOutput.ErrorHelp1.Caption :='Fehlerhilfe...';
        BASinOutput.N3.Caption :='-';
        BASinOutput.About1.Caption :='Über...';
        BASinOutput.Token1.Caption :='Token';
        BASinOutput.Help2.Caption :='Token-Hilfe...';
        BASinOutput.Tokenise1.Caption :='In Token umwandeln';
        BASinOutput.EditVariable1.Caption :='Variable bearbeiten...';
        BASinOutput.FindLine1.Caption :='Zeile finden';
        BASinOutput.StringOperation1.Caption :='String Operation';
        BASinOutput.Wordwrapstring1.Caption :='Wortumbruch';
        BASinOutput.Splitat32chars1.Caption :='Umbruch nach 32 Zeichen';
        BASinOutput.Insertspaces1.Caption :='Leerzeichen einfügen';
        BASinOutput.Tokeniseall1.Caption :='alles in Token umwandeln';
        BASinOutput.Detokeniseall1.Caption :='Detokenise all';
        BASinOutput.N16.Caption :='-';
        BASinOutput.Cut2.Caption :='Ausschneiden';
        BASinOutput.Copy2.Caption :='Kopieren';
        BASinOutput.Paste2.Caption :='Einfügen';
        BASinOutput.N17.Caption :='-';
        BASinOutput.Debug1.Caption :='Debug';
        BASinOutput.ToggleBreakpoint1.Caption :='Haltepunkt umschalten';
        BASinOutput.RunToCursor1.Caption :='Laufe bis Zeile';
        BASinOutput.GoToCursor1.Caption :='Gehe zu Zeile';
        BASinOutput.WatchVariable1.Caption :='Überwache Variable';

// Missing BasinMain captions (from BasinMain.dfm)
        BASinOutput.Label2.Caption :='Gehe zu Sub';
        BASinOutput.LoadBasinState1.Caption :='Arbeitsbereich öffnen...';
        BASinOutput.ExportTap1.Caption :='Tap exportieren...';
        BASinOutput.SendtoExternalUtiility1.Caption :='SNA an externes Tool senden...';
        BASinOutput.SaveWorkspace1.Caption :='Arbeitsbereich speichern...';
        BASinOutput.CopyAsPlainText2.Caption :='Als reinen Text kopieren';
        BASinOutput.SaveListingasImage1.Caption :='Listing als Bild speichern';
        BASinOutput.SubRoutineList1.Caption :='Unterprogramm-Liste';
        BASinOutput.EnableSubList1.Caption :='Sub-Liste aktivieren';
        BASinOutput.N24.Caption :='-';
        BASinOutput.AutoDetectGoSubs1.Caption :='Go Subs autom. erkennen';
        BASinOutput.AutoDetectGOTOs2.Caption :='GO TOs autom. erkennen';
        BASinOutput.PreviewOrigins1.Caption :='Sprung-Ursprünge zeigen';
        BASinOutput.InfoLine1.Caption :='Info-Zeile';
        BASinOutput.CodeControlIcons1.Caption :='Haltepunkt-Steuerungssymbole';
        BASinOutput.SmartIndenting1.Caption :='Intelligentes Einrücken';
        BASinOutput.SnaptoEditor1.Caption :='An Editor-Fenster andocken';
        BASinOutput.AlwaysOnTop1.Caption :='Immer im Vordergrund';
        BASinOutput.PrintOutput1.Caption :='Bildschirmtext-Erfassung';
        BASinOutput.Notes1.Caption :='Wettbewerbsgröße anzeigen';
        BASinOutput.AIChatPanel1.Caption :='KI-Chat-Panel';
        BASinOutput.BinaryImportExport1.Caption :='Binärer Import/Export';
        BASinOutput.OptimizeBasic1.Caption :='Basic bereinigen/minimieren';
        BASinOutput.N20.Caption :='-';
        BASinOutput.simplecon1.Caption :='SimpleCon-Schnittstelle';
        BASinOutput.N22.Caption :='-';
        BASinOutput.Editors1.Caption :='Editoren';
        BASinOutput.GraphEditor1.Caption :='Cheq Grafik-Editor';
        BASinOutput.SnippetEditor1.Caption :='Snippet-Editor';
        BASinOutput.N27.Caption :='-';
        BASinOutput.ProjectNotesEditor1.Caption :='Projektnotizen-Editor';
        BASinOutput.UlaPlusPaletteEditor1.Caption :='UlaPlus Paletten-Editor';
        BASinOutput.ShowaTip1.Caption :='Tipp anzeigen';
        BASinOutput.BasinCOfficialDiscordServer1.Caption :='Offizieller BasinC Discord-Server';
        BASinOutput.BasinCShare1.Caption :='BasinC Teilen...';
        BASinOutput.CheqEditPage1.Caption :='CheqEdit Grafik-Editor holen';
        BASinOutput.GetPasmo1.Caption :='Pasmo-0.5.3 Windows holen';
        BASinOutput.N23.Caption :='-';
        BASinOutput.CheckUpdates1.Caption :='Auf Updates prüfen';
        BASinOutput.N25.Caption :='-';
        BASinOutput.JumpTargets1.Caption :='Sprünge hierher zurückverfolgen';
        BASinOutput.EnableAutoSub1.Caption :='(Auto-Sub-Erkennung ist deaktiviert)';
        BASinOutput.CopyAsPlainText1.Caption :='Kopieren';
        BASinOutput.SourceMarkers2.Caption :='Quellcode-Markierungen';
        BASinOutput.SetMarker2.Caption :='Markierung setzen';
        BASinOutput.Marker04.Caption :='Markierung 0';
        BASinOutput.Marker14.Caption :='Markierung 1';
        BASinOutput.Marker24.Caption :='Markierung 2';
        BASinOutput.Marker34.Caption :='Markierung 3';
        BASinOutput.Marker44.Caption :='Markierung 4';
        BASinOutput.Marker54.Caption :='Markierung 5';
        BASinOutput.Marker64.Caption :='Markierung 6';
        BASinOutput.Marker74.Caption :='Markierung 7';
        BASinOutput.Marker84.Caption :='Markierung 8';
        BASinOutput.Marker94.Caption :='Markierung 9';
        BASinOutput.GetMarker2.Caption :='Gehe zu Markierung';
        BASinOutput.Marker03.Caption :='Markierung 0';
        BASinOutput.Marker13.Caption :='Markierung 1';
        BASinOutput.Marker23.Caption :='Markierung 2';
        BASinOutput.Marker33.Caption :='Markierung 3';
        BASinOutput.Marker43.Caption :='Markierung 4';
        BASinOutput.Marker53.Caption :='Markierung 5';
        BASinOutput.Marker63.Caption :='Markierung 6';
        BASinOutput.Marker73.Caption :='Markierung 7';
        BASinOutput.Marker83.Caption :='Markierung 8';
        BASinOutput.Marker93.Caption :='Markierung 9';
        BASinOutput.N26.Caption :='-';
        BASinOutput.Clearall2.Caption :='Alle löschen';
        BASinOutput.AddNote1.Caption :='Notiz hinzufügen';
        BASinOutput.AddSnippet1.Caption :='Zu Snippets hinzufügen';
        BASinOutput.Share1.Caption :='Teilen...';
        // Missing BasinMain hints (from BasinMain.dfm)
        BASinOutput.SpeedButton1.Hint :='Neues Programm erstellen|';
        BASinOutput.SpeedButton2.Hint :='Programm laden|';
        BASinOutput.SpeedButton3.Hint :='Aktuelles Programm speichern|';
        BASinOutput.SpeedButton5.Hint :='Vom Cursor starten|';
        BASinOutput.SpeedButton11.Hint :='Überwachung hinzufügen|';
        BASinOutput.SpeedButton12.Hint :='Auswerten|';
        BASinOutput.SpeedButton10.Hint :='Haltepunkt am Cursor setzen|';
        BASinOutput.SpeedButton8.Hint :='Überspringen (Step Over)|';
        BASinOutput.SpeedButton7.Hint :='Einzelschritt|';
        BASinOutput.SpeedButton9.Hint :='Bis zum Cursor ausführen|';
        BASinOutput.New1.Hint :='Erstelle ein neues Programm';
        BASinOutput.Load1.Hint :='Lade ein Programm';
        BASinOutput.ReLOAD1.Hint :='Eine Liste der zuletzt geladenen Programme';
        BASinOutput.PreviousSession1.Hint :='BASin auf den Stand der letzten Sitzung zurücksetzen.';
        BASinOutput.ImportBASIC1.Hint :='BASIC- oder CODE-Daten aus beliebiger Datei lesen';
        BASinOutput.ImportfromTapeImage1.Hint :='Virtuelles Kassettenabbild einlegen und davon laden';
        BASinOutput.Save1.Hint :='Das aktuelle Programm speichern';
        BASinOutput.SaveBASICas1.Hint :='Das aktuelle Programm aus dem Speicher extrahieren';
        BASinOutput.Print1.Hint :='Das aktuelle Programm drucken';
        BASinOutput.Exit1.Hint :='BASin beenden';
        BASinOutput.Undo1.Hint :='Letzte Programmänderung rückgängig machen';
        BASinOutput.Redo1.Hint :='Rückgängig gemachte Änderungen wiederherstellen';
        BASinOutput.Cut1.Hint :='Ausgewählten Text entfernen und in die Zwischenablage kopieren';
        BASinOutput.Copy1.Hint :='Ausgewählten Text in die Zwischenablage kopieren';
        BASinOutput.CopyAsPlainText2.Hint :='Ausgewählten Text in die Zwischenablage kopieren';
        BASinOutput.Paste1.Hint :='Text aus der Zwischenablage in die Bearbeitungszeile einfügen';
        BASinOutput.Delete1.Hint :='Ausgewählten Text aus der Bearbeitungszeile entfernen';
        BASinOutput.CopyListing1.Hint :='Das gesamte Programm in die Zwischenablage kopieren';
        BASinOutput.DebugWindows1.Hint :='Fenster für das Debugging';
        BASinOutput.Variables1.Hint :='Aktuell deklarierte Variablen anzeigen';
        BASinOutput.SystemVariables1.Hint :='Aktuelle Systemvariablen anzeigen';
        BASinOutput.Breakpoints1.Hint :='Aktuell verwendete Haltepunkte anzeigen';
        BASinOutput.Watches1.Hint :='Verwendete Überwachungsbedingungen auflisten';
        BASinOutput.GOSUBStack1.Hint :='GO SUB Stack anzeigen';
        BASinOutput.MemoryMap1.Hint :='Zeigt eine visuelle Darstellung der Speichernutzung.';
        BASinOutput.MemoryViewer1.Hint :='Spectrum-Speicher anzeigen oder bearbeiten';
        BASinOutput.LogWindow1.Hint :='Protokollierte Debug-Ausgaben anzeigen.';
        BASinOutput.ProfileResults1.Hint :='Ergebnisse der letzten Code-Profilierung anzeigen';
        BASinOutput.CPUWindow1.Hint :='CPU-Fenster zum Debuggen von Assembler-Code anzeigen';
        BASinOutput.SubRoutineList1.Hint :='Zeigt eine Liste der durch REM #regionname definierten Orte';
        BASinOutput.SyntaxHelper1.Hint :='BASIC-Syntaxprüfung anzeigen';
        BASinOutput.CharacterRuler1.Hint :='Zeigt ein kleines "Lineal", das String-Längen darstellt';
        BASinOutput.CodeControlIcons1.Hint :='Haltepunkt-Schaltflächen in der Symbolleiste anzeigen';
        BASinOutput.WindowSize1.Hint :='Größe von BASin ändern';
        BASinOutput.DisplayWindow1.Hint :='Laufzeit-Ausgabefenster anzeigen oder verbergen';
        BASinOutput.SnaptoEditor1.Hint :='Ausgabefenster an Editor-Fenster andocken';
        BASinOutput.N100320x2401.Hint :='Normale (1:1) Fenstergröße';
        BASinOutput.N200640x4801.Hint :='Doppelte (2:1) Fenstergröße';
        BASinOutput.Custom1.Hint :='Die aktuellen Abmessungen';
        BASinOutput.Force11Aspect1.Hint :='BASin zwingen, das korrekte Seitenverhältnis beizubehalten';
        BASinOutput.ExpressionEvaluator1.Hint :='Taschenrechner-Fenster anzeigen';
        BASinOutput.ProgramInformation1.Hint :='Informationen über das aktuelle Programm anzeigen';
        BASinOutput.CommandHistory1.Hint :='Verlauf der Bearbeitungszeile auflisten';
        BASinOutput.LastError1.Hint :='Letzte Laufzeit-Fehlermeldung anzeigen';
        BASinOutput.Find1.Hint :='Text im aktuellen Programm suchen';
        BASinOutput.Replace1.Hint :='Text im aktuellen Programm ersetzen';
        BASinOutput.FindNext1.Hint :='Letzten Suchvorgang wiederholen';
        BASinOutput.ReplaceNext1.Hint :='Letzten Ersetzungsvorgang wiederholen';
        BASinOutput.SourceMarkers1.Hint :='Mit Quellcode-Markierungen arbeiten';
        BASinOutput.SetMarker1.Hint :='Quellcode-Markierungen setzen';
        BASinOutput.Marker01.Hint :='Quellcode-Markierung 0 setzen';
        BASinOutput.Marker11.Hint :='Quellcode-Markierung 1 setzen';
        BASinOutput.Marker21.Hint :='Quellcode-Markierung 2 setzen';
        BASinOutput.Marker31.Hint :='Quellcode-Markierung 3 setzen';
        BASinOutput.Marker41.Hint :='Quellcode-Markierung 4 setzen';
        BASinOutput.Marker51.Hint :='Quellcode-Markierung 5 setzen';
        BASinOutput.Marker61.Hint :='Quellcode-Markierung 6 setzen';
        BASinOutput.Marker71.Hint :='Quellcode-Markierung 7 setzen';
        BASinOutput.Marker81.Hint :='Quellcode-Markierung 8 setzen';
        BASinOutput.Marker91.Hint :='Quellcode-Markierung 9 setzen';
        BASinOutput.GetMarker1.Hint :='Zu einer Quellcode-Markierung springen';
        BASinOutput.Marker02.Hint :='Zu Quellcode-Markierung 0 springen';
        BASinOutput.Marker12.Hint :='Zu Quellcode-Markierung 1 springen';
        BASinOutput.Marker22.Hint :='Zu Quellcode-Markierung 2 springen';
        BASinOutput.Marker32.Hint :='Zu Quellcode-Markierung 3 springen';
        BASinOutput.Marker42.Hint :='Zu Quellcode-Markierung 4 springen';
        BASinOutput.Marker52.Hint :='Zu Quellcode-Markierung 5 springen';
        BASinOutput.Marker62.Hint :='Zu Quellcode-Markierung 6 springen';
        BASinOutput.Marker72.Hint :='Zu Quellcode-Markierung 7 springen';
        BASinOutput.Marker82.Hint :='Zu Quellcode-Markierung 8 springen';
        BASinOutput.Marker92.Hint :='Zu Quellcode-Markierung 9 springen';
        BASinOutput.Clearall1.Hint :='Alle Quellcode-Markierungen löschen';
        BASinOutput.GotoLineNumber1.Hint :='Programm-Cursor auf eine Zeilennummer setzen';
        BASinOutput.GotoError1.Hint :='Letzte fehlerhafte Anweisung finden';
        BASinOutput.Run2.Hint :='Programm starten oder stoppen';
        BASinOutput.GOTO1.Hint :='Programm ab einer bestimmten Zeilennummer starten';
        BASinOutput.EnableProfiling1.Hint :='Profiling-System aktivieren, um Code-Laufzeit zu messen';
        BASinOutput.ForceBREAK1.Hint :='Kehrt zum Editor zurück.';
        BASinOutput.TraceExecution1.Hint :='Programmausführung zur Laufzeit visuell im Editor anzeigen';
        BASinOutput.SingleStepStatement1.Hint :='Schrittweise durch das Programm gehen';
        BASinOutput.StepToNext1.Hint :='Anweisungen ausführen, bis die nächste Zeile erreicht ist';
        BASinOutput.RunTo1.Hint :='Zeilen ausführen, bis eine bestimmte Zeile erreicht ist';
        BASinOutput.oggleBreakpoint1.Hint :='Haltepunkt an der aktuellen Zeile umschalten';
        BASinOutput.AddBreakpoint1.Hint :='Einen Haltepunkt zur Liste hinzufügen';
        BASinOutput.AddWatch1.Hint :='Eine Bedingung zur Überwachungsliste hinzufügen';
        BASinOutput.BASinOptions1.Hint :='BASin konfigurieren';
        BASinOutput.TokenTable1.Hint :='Erlaubt Token-Eingabe per Einzelklick';
        BASinOutput.Renumber1.Hint :='Aktuelles Programm neu nummerieren';
        BASinOutput.ZXPrinterOutput1.Hint :='Ausgabe des emulierten ZX Printers anzeigen';
        BASinOutput.Assembler1.Hint :='Integrierten Z80-Assembler öffnen';
        BASinOutput.Compiler1.Hint :='Aktuelles Programm in ASM-Text kompilieren';
        BASinOutput.UDGEditor1.Hint :='UDGs/Zeichen grafisch bearbeiten';
        BASinOutput.ScreenPaintbox1.Hint :='Bildschirm-Malwerkzeug';
        BASinOutput.TapeCreator1.Hint :='Kassettendatei (.tap/.tzx/.wav) erstellen';
        BASinOutput.BEEPComposer1.Hint :='BEEP-Melodien visuell erstellen';
        BASinOutput.Contents1.Hint :='BASin-Hilfedatei anzeigen';
        BASinOutput.CommandHelp1.Hint :='Hilfe zum aktuellen Befehl erhalten';
        BASinOutput.SinclairBASICManual1.Hint :='Sinclair BASIC-Handbuch anzeigen';
        BASinOutput.ErrorHelp1.Hint :='Hilfe zu Laufzeitfehlern erhalten';
        BASinOutput.About1.Hint :='Über BASin';
        BASinOutput.SourceMarkers2.Hint :='Mit Quellcode-Markierungen arbeiten';
        BASinOutput.SetMarker2.Hint :='Quellcode-Markierungen setzen';
        BASinOutput.Marker04.Hint :='Quellcode-Markierung 0 setzen';
        BASinOutput.Marker14.Hint :='Quellcode-Markierung 1 setzen';
        BASinOutput.Marker24.Hint :='Quellcode-Markierung 2 setzen';
        BASinOutput.Marker34.Hint :='Quellcode-Markierung 3 setzen';
        BASinOutput.Marker44.Hint :='Quellcode-Markierung 4 setzen';
        BASinOutput.Marker54.Hint :='Quellcode-Markierung 5 setzen';
        BASinOutput.Marker64.Hint :='Quellcode-Markierung 6 setzen';
        BASinOutput.Marker74.Hint :='Quellcode-Markierung 7 setzen';
        BASinOutput.Marker84.Hint :='Quellcode-Markierung 8 setzen';
        BASinOutput.Marker94.Hint :='Quellcode-Markierung 9 setzen';
        BASinOutput.GetMarker2.Hint :='Zu einer Quellcode-Markierung springen';
        BASinOutput.Marker03.Hint :='Zu Quellcode-Markierung 0 springen';
        BASinOutput.Marker13.Hint :='Zu Quellcode-Markierung 1 springen';
        BASinOutput.Marker23.Hint :='Zu Quellcode-Markierung 2 springen';
        BASinOutput.Marker33.Hint :='Zu Quellcode-Markierung 3 springen';
        BASinOutput.Marker43.Hint :='Zu Quellcode-Markierung 4 springen';
        BASinOutput.Marker53.Hint :='Zu Quellcode-Markierung 5 springen';
        BASinOutput.Marker63.Hint :='Zu Quellcode-Markierung 6 springen';
        BASinOutput.Marker73.Hint :='Zu Quellcode-Markierung 7 springen';
        BASinOutput.Marker83.Hint :='Zu Quellcode-Markierung 8 springen';
        BASinOutput.Marker93.Hint :='Zu Quellcode-Markierung 9 springen';
        BASinOutput.Clearall2.Hint :='Alle Quellcode-Markierungen löschen';
        end;



if (lang='Spanish') then
        Begin
                  // urceR14.20110529\BasinMain.dfm
        BASinOutput.Caption :=ReleaseName;
        //BASinOutput.Label1.Caption :='Label1';
        BASinOutput.File1.Caption :='&Archivo';
        BASinOutput.New1.Caption :='Nuevo';
        BASinOutput.N5.Caption :='-';
        BASinOutput.Load1.Caption :='Abrir';
        BASinOutput.ReLOAD1.Caption :='Reabrir';
        BASinOutput.PreviousSession1.Caption :='Sesión anterior';
        BASinOutput.N12.Caption :='-';
        BASinOutput.Item11.Caption :='Item1';
        BASinOutput.Item21.Caption :='Item2';
        BASinOutput.Item31.Caption :='Item3';
        BASinOutput.Item41.Caption :='Item4';
        BASinOutput.Item51.Caption :='Item5';
        BASinOutput.Item61.Caption :='Item6';
        BASinOutput.Item71.Caption :='Item7';
        BASinOutput.Item81.Caption :='Item8';
        BASinOutput.ImportBASIC1.Caption :='Importar Binario...';
        BASinOutput.ImportfromTapeImage1.Caption :='Insertar Cinta...';
        BASinOutput.N1.Caption :='-';
        BASinOutput.Save1.Caption :='Guardar';
        BASinOutput.SaveBASICas1.Caption :='Guardar Como...';
        BASinOutput.N2.Caption :='-';
        BASinOutput.Print1.Caption :='Imprimir...';
        BASinOutput.N9.Caption :='-';
        BASinOutput.Exit1.Caption :='Salir';
        BASinOutput.Edit1.Caption :='Editar';
        BASinOutput.Undo1.Caption :='Deshacer';
        BASinOutput.Redo1.Caption :='Rehacer';
        BASinOutput.N6.Caption :='-';
        BASinOutput.Cut1.Caption :='Cortar';
        BASinOutput.Copy1.Caption :='Special Copiar';
        BASinOutput.Paste1.Caption :='Pegar';
        BASinOutput.Delete1.Caption :='Borrar';
        BASinOutput.N14.Caption :='-';
        BASinOutput.CopyListing1.Caption :='Copiar Listado';
        BASinOutput.View1.Caption :='&Ver';
        BASinOutput.ToolBar1.Caption :='Barra Herramientas';
        BASinOutput.StatusBar2.Caption :='Barra Estado';
        BASinOutput.SyntaxHelper1.Caption :='Ayuda sintaxis';
        BASinOutput.CharacterRuler1.Caption :='Regla Caracteres';
        BASinOutput.N15.Caption :='-';
        BASinOutput.ProgramInformation1.Caption :='Información del Programa';
        BASinOutput.ZXPrinterOutput1.Caption :='Salida Impresora ZX';
        BASinOutput.CommandHistory1.Caption :='Histórico de Comandos';
        BASinOutput.LastError1.Caption :='Último Error';
        BASinOutput.DebugWindows1.Caption :='Ventanas Depuración';
        BASinOutput.Variables1.Caption :='Variables';
        BASinOutput.SystemVariables1.Caption :='Variables de Sistema';
        BASinOutput.Breakpoints1.Caption :='Puntos Parada';
        BASinOutput.Watches1.Caption :='Puntos Observación';
        BASinOutput.GOSUBStack1.Caption :='Pila GO SUB';
        BASinOutput.MemoryMap1.Caption :='Mapa Memoria';
        BASinOutput.MemoryViewer1.Caption :='Visor Memoria';
        BASinOutput.LogWindow1.Caption :='Ventana Registro';
        BASinOutput.ProfileResults1.Caption :='Resultados Perfilador';
        BASinOutput.CPUWindow1.Caption :='Ventana CPU';
        BASinOutput.WindowSize1.Caption :='Ventana Visualización';
        BASinOutput.DisplayWindow1.Caption :='Visible';
        BASinOutput.N100320x2401.Caption :='100% (320x240)';
        BASinOutput.N200640x4801.Caption :='200% (640x480)';
        BASinOutput.Custom1.Caption :='Personalizado ()';
        BASinOutput.N13.Caption :='-';
        BASinOutput.Force11Aspect1.Caption :='Forzar Aspecto 1:1';
        BASinOutput.ExpressionEvaluator1.Caption :='Evaluador de Expresiones';
        BASinOutput.Search1.Caption :='Buscar';
        BASinOutput.Find1.Caption :='Buscar...';
        BASinOutput.Replace1.Caption :='Reemplazar...';
        BASinOutput.FindNext1.Caption :='Buscar Siguiente';
        BASinOutput.ReplaceNext1.Caption :='Reemplazar Siguiente';
        BASinOutput.N10.Caption :='-';
        BASinOutput.SourceMarkers1.Caption :='Marcadores de Código';
        BASinOutput.SetMarker1.Caption :='Establecer Marcador';
        BASinOutput.Marker01.Caption :='Marcador 0';
        BASinOutput.Marker11.Caption :='Marcador 1';
        BASinOutput.Marker21.Caption :='Marcador 2';
        BASinOutput.Marker31.Caption :='Marcador 3';
        BASinOutput.Marker41.Caption :='Marcador 4';
        BASinOutput.Marker51.Caption :='Marcador 5';
        BASinOutput.Marker61.Caption :='Marcador 6';
        BASinOutput.Marker71.Caption :='Marcador 7';
        BASinOutput.Marker81.Caption :='Marcador 8';
        BASinOutput.Marker91.Caption :='Marcador 9';
        BASinOutput.GetMarker1.Caption :='Obtener Marcador';
        BASinOutput.Marker02.Caption :='Marcador 0';
        BASinOutput.Marker12.Caption :='Marcador 1';
        BASinOutput.Marker22.Caption :='Marcador 2';
        BASinOutput.Marker32.Caption :='Marcador 3';
        BASinOutput.Marker42.Caption :='Marcador 4';
        BASinOutput.Marker52.Caption :='Marcador 5';
        BASinOutput.Marker62.Caption :='Marcador 6';
        BASinOutput.Marker72.Caption :='Marcador 7';
        BASinOutput.Marker82.Caption :='Marcador 8';
        BASinOutput.Marker92.Caption :='Marcador 9';
        BASinOutput.N18.Caption :='-';
        BASinOutput.Clearall1.Caption :='Limpiar todo';
        BASinOutput.GotoLineNumber1.Caption :='Ir a Línea...';
        BASinOutput.GotoError1.Caption :='Ir a Error';
        BASinOutput.Run1.Caption :='Ejecutar';
        BASinOutput.Run2.Caption :='Ejecutar';
        BASinOutput.Continue1.Caption :='Continuar';
        BASinOutput.GOTO1.Caption :='Ir al Cursor';
        BASinOutput.EnableProfiling1.Caption :='Habilitar Perfilado';
        BASinOutput.N11.Caption :='-';
        BASinOutput.ForceBREAK1.Caption :='Forzar BREAK';
        BASinOutput.N7.Caption :='-';
        BASinOutput.TraceExecution1.Caption :='Trazar Ejecución';
        BASinOutput.SingleStepStatement1.Caption :='Avanzar Instrucción';
        BASinOutput.StepToNext1.Caption :='Volver Instrucción';
        BASinOutput.RunTo1.Caption :='Ejecutar Hasta Cursor';
        BASinOutput.N8.Caption :='-';
        BASinOutput.oggleBreakpoint1.Caption :='Cambiar Punto Ruptura';
        BASinOutput.AddBreakpoint1.Caption :='Añadir Punto Ruptura...';
        BASinOutput.AddWatch1.Caption :='Añadir Punto Observación...';
        BASinOutput.N19.Caption :='-';
        BASinOutput.FullSpeed1.Caption :='Emulación a Toda Velocidad';
        BASinOutput.Tools1.Caption :='Herramientas';
        BASinOutput.BASinOptions1.Caption :='Opciones BASin...';
        BASinOutput.N4.Caption :='-';
        BASinOutput.TokenTable1.Caption :='Tabla de Token...';
        BASinOutput.BEEPComposer1.Caption :='Compositor BEEP...';
        BASinOutput.Memorygrabber1.Caption :='Capturador Memoria...';
        BASinOutput.UDGEditor1.Caption :='Editor Gráfico...';
        BASinOutput.ScreenPaintbox1.Caption :='Pintar Pantalla...';
        BASinOutput.Renumber1.Caption :='Renumerar...';
        BASinOutput.TapeCreator1.Caption :='Creador de Cintas...';
        BASinOutput.Compiler1.Caption :='Compilador';
        BASinOutput.Assembler1.Caption :='Ensamblador';
        BASinOutput.MemoryEditor1.Caption :='Editor Memoria';
        BASinOutput.Help1.Caption :='Ayuda';
        BASinOutput.Contents1.Caption :='Contenido...';
        BASinOutput.CommandHelp1.Caption :='Ayuda de Comandos';
        BASinOutput.SinclairBASICManual1.Caption :='Manual Sinclair BASIC';
        BASinOutput.ErrorHelp1.Caption :='Ayuda de Errores...';
        BASinOutput.N3.Caption :='-';
        BASinOutput.About1.Caption :='Acerca de...';
        BASinOutput.Token1.Caption :='Token';
        BASinOutput.Help2.Caption :='Ayuda de Tokens...';
        BASinOutput.Tokenise1.Caption :='Reconocer tokens';
        BASinOutput.EditVariable1.Caption :='Editar Variable...';
        BASinOutput.FindLine1.Caption :='Buscar Línea';
        BASinOutput.StringOperation1.Caption :='Operaciones de Cadenas';
        BASinOutput.Wordwrapstring1.Caption :='Ajuste Línea';
        BASinOutput.Splitat32chars1.Caption :='Recortar a los 32 carácteres';
        BASinOutput.Insertspaces1.Caption :='Insertar espacios';
        BASinOutput.Tokeniseall1.Caption :='Reconocer tokens';
        BASinOutput.Detokeniseall1.Caption :='Deshacer tokens';
        BASinOutput.N16.Caption :='-';
        BASinOutput.Cut2.Caption :='Cortar';
        BASinOutput.Copy2.Caption :='Copiar';
        BASinOutput.Paste2.Caption :='Pegar';
        BASinOutput.N17.Caption :='-';
        BASinOutput.Debug1.Caption :='Depurar';
        BASinOutput.ToggleBreakpoint1.Caption :='Cambiar Punto Ruptura';
        BASinOutput.RunToCursor1.Caption :='Ejecutar Hasta Línea';
        BASinOutput.GoToCursor1.Caption :='Ir a Línea';
        BASinOutput.WatchVariable1.Caption :='Observar Variable';

// Missing BasinMain captions (from BasinMain.dfm)
        BASinOutput.Label2.Caption :='Ir a sub';
        BASinOutput.LoadBasinState1.Caption :='Abrir espacio de trabajo...';
        BASinOutput.ExportTap1.Caption :='Exportar Tap...';
        BASinOutput.SendtoExternalUtiility1.Caption :='Enviar SNA a utilidad externa...';
        BASinOutput.SaveWorkspace1.Caption :='Guardar espacio de trabajo...';
        BASinOutput.CopyAsPlainText2.Caption :='Copiar como texto plano';
        BASinOutput.SaveListingasImage1.Caption :='Guardar listado como imagen';
        BASinOutput.SubRoutineList1.Caption :='Lista de subrutinas';
        BASinOutput.EnableSubList1.Caption :='Habilitar lista de subrutinas';
        BASinOutput.N24.Caption :='-';
        BASinOutput.AutoDetectGoSubs1.Caption :='Autodetectar Go Subs';
        BASinOutput.AutoDetectGOTOs2.Caption :='Autodetectar GO TOs';
        BASinOutput.PreviewOrigins1.Caption :='Mostrar orígenes de salto';
        BASinOutput.InfoLine1.Caption :='Línea de información';
        BASinOutput.CodeControlIcons1.Caption :='Iconos de control de puntos de ruptura';
        BASinOutput.SmartIndenting1.Caption :='Indentación inteligente';
        BASinOutput.SnaptoEditor1.Caption :='Acoplar a ventana del editor';
        BASinOutput.AlwaysOnTop1.Caption :='Siempre visible';
        BASinOutput.PrintOutput1.Caption :='Captura de texto de pantalla';
        BASinOutput.Notes1.Caption :='Mostrar tamaño para competición';
        BASinOutput.AIChatPanel1.Caption :='Panel de chat IA';
        BASinOutput.BinaryImportExport1.Caption :='Importar/Exportar binario';
        BASinOutput.OptimizeBasic1.Caption :='Limpiar/Minimizar Basic';
        BASinOutput.N20.Caption :='-';
        BASinOutput.simplecon1.Caption :='Interfaz SimpleCon';
        BASinOutput.N22.Caption :='-';
        BASinOutput.Editors1.Caption :='Editores';
        BASinOutput.GraphEditor1.Caption :='Editor gráfico Cheq';
        BASinOutput.SnippetEditor1.Caption :='Editor de fragmentos';
        BASinOutput.N27.Caption :='-';
        BASinOutput.ProjectNotesEditor1.Caption :='Editor de notas del proyecto';
        BASinOutput.UlaPlusPaletteEditor1.Caption :='Editor de paleta UlaPlus';
        BASinOutput.ShowaTip1.Caption :='Mostrar un consejo';
        BASinOutput.BasinCOfficialDiscordServer1.Caption :='Servidor oficial de Discord de BasinC';
        BASinOutput.BasinCShare1.Caption :='Compartir BasinC...';
        BASinOutput.CheqEditPage1.Caption :='Obtener editor gráfico CheqEdit';
        BASinOutput.GetPasmo1.Caption :='Obtener Pasmo-0.5.3 Windows';
        BASinOutput.N23.Caption :='-';
        BASinOutput.CheckUpdates1.Caption :='Buscar actualizaciones';
        BASinOutput.N25.Caption :='-';
        BASinOutput.JumpTargets1.Caption :='Rastrear saltos hasta aquí';
        BASinOutput.EnableAutoSub1.Caption :='(Autodetección de subrutinas desactivada)';
        BASinOutput.CopyAsPlainText1.Caption :='Copiar';
        BASinOutput.SourceMarkers2.Caption :='Marcadores de código';
        BASinOutput.SetMarker2.Caption :='Establecer marcador';
        BASinOutput.Marker04.Caption :='Marcador 0';
        BASinOutput.Marker14.Caption :='Marcador 1';
        BASinOutput.Marker24.Caption :='Marcador 2';
        BASinOutput.Marker34.Caption :='Marcador 3';
        BASinOutput.Marker44.Caption :='Marcador 4';
        BASinOutput.Marker54.Caption :='Marcador 5';
        BASinOutput.Marker64.Caption :='Marcador 6';
        BASinOutput.Marker74.Caption :='Marcador 7';
        BASinOutput.Marker84.Caption :='Marcador 8';
        BASinOutput.Marker94.Caption :='Marcador 9';
        BASinOutput.GetMarker2.Caption :='Ir a marcador';
        BASinOutput.Marker03.Caption :='Marcador 0';
        BASinOutput.Marker13.Caption :='Marcador 1';
        BASinOutput.Marker23.Caption :='Marcador 2';
        BASinOutput.Marker33.Caption :='Marcador 3';
        BASinOutput.Marker43.Caption :='Marcador 4';
        BASinOutput.Marker53.Caption :='Marcador 5';
        BASinOutput.Marker63.Caption :='Marcador 6';
        BASinOutput.Marker73.Caption :='Marcador 7';
        BASinOutput.Marker83.Caption :='Marcador 8';
        BASinOutput.Marker93.Caption :='Marcador 9';
        BASinOutput.N26.Caption :='-';
        BASinOutput.Clearall2.Caption :='Borrar todo';
        BASinOutput.AddNote1.Caption :='Añadir nota';
        BASinOutput.AddSnippet1.Caption :='Añadir a fragmentos';
        BASinOutput.Share1.Caption :='Compartir...';
        // Missing BasinMain hints (from BasinMain.dfm)
        BASinOutput.SpeedButton1.Hint :='Crear un programa nuevo|';
        BASinOutput.SpeedButton2.Hint :='Cargar un programa|';
        BASinOutput.SpeedButton3.Hint :='Guardar el programa actual|';
        BASinOutput.SpeedButton5.Hint :='Iniciar desde el cursor|';
        BASinOutput.SpeedButton11.Hint :='Añadir inspección|';
        BASinOutput.SpeedButton12.Hint :='Evaluar|';
        BASinOutput.SpeedButton10.Hint :='Añadir punto de ruptura en cursor|';
        BASinOutput.SpeedButton8.Hint :='Paso por encima|';
        BASinOutput.SpeedButton7.Hint :='Paso a paso|';
        BASinOutput.SpeedButton9.Hint :='Ejecutar hasta el cursor|';
        BASinOutput.New1.Hint :='Crear un programa nuevo';
        BASinOutput.Load1.Hint :='Cargar un programa';
        BASinOutput.ReLOAD1.Hint :='Una lista de programas cargados recientemente';
        BASinOutput.PreviousSession1.Hint :='Restaurar BASin a la configuración de la última sesión.';
        BASinOutput.ImportBASIC1.Hint :='Leer datos BASIC o CODE desde cualquier archivo';
        BASinOutput.ImportfromTapeImage1.Hint :='Insertar una imagen de cinta virtual y cargar desde ella';
        BASinOutput.Save1.Hint :='Guardar el programa actual';
        BASinOutput.SaveBASICas1.Hint :='Extraer el programa actual de la memoria';
        BASinOutput.Print1.Hint :='Imprimir el programa actual';
        BASinOutput.Exit1.Hint :='Salir de BASin';
        BASinOutput.Undo1.Hint :='Deshacer el último cambio del programa';
        BASinOutput.Redo1.Hint :='Rehacer los últimos cambios tras deshacer';
        BASinOutput.Cut1.Hint :='Cortar el texto seleccionado al portapapeles';
        BASinOutput.Copy1.Hint :='Copiar el texto seleccionado al portapapeles';
        BASinOutput.CopyAsPlainText2.Hint :='Copiar el texto seleccionado al portapapeles';
        BASinOutput.Paste1.Hint :='Pegar texto del portapapeles en la línea de edición';
        BASinOutput.Delete1.Hint :='Eliminar el texto seleccionado de la línea de edición';
        BASinOutput.CopyListing1.Hint :='Copiar todo el programa al portapapeles';
        BASinOutput.DebugWindows1.Hint :='Ventanas utilizadas para depuración';
        BASinOutput.Variables1.Hint :='Mostrar las variables declaradas actualmente';
        BASinOutput.SystemVariables1.Hint :='Mostrar las variables del sistema actuales';
        BASinOutput.Breakpoints1.Hint :='Ver puntos de ruptura actualmente en uso';
        BASinOutput.Watches1.Hint :='Listar condiciones de inspección en uso';
        BASinOutput.GOSUBStack1.Hint :='Mostrar la pila de GO SUB';
        BASinOutput.MemoryMap1.Hint :='Muestra una visualización del uso de memoria.';
        BASinOutput.MemoryViewer1.Hint :='Ver o editar la memoria del Spectrum';
        BASinOutput.LogWindow1.Hint :='Ver cualquier salida de depuración registrada.';
        BASinOutput.ProfileResults1.Hint :='Mostrar los resultados de la última operación de perfilado';
        BASinOutput.CPUWindow1.Hint :='Mostrar la ventana CPU para depurar código ensamblador';
        BASinOutput.SubRoutineList1.Hint :='Muestra una lista de lugares definidos por REM #regionname';
        BASinOutput.SyntaxHelper1.Hint :='Mostrar el verificador de sintaxis BASIC';
        BASinOutput.CharacterRuler1.Hint :='Muestra una pequeña "regla" que indica longitudes de cadena';
        BASinOutput.CodeControlIcons1.Hint :='Mostrar botones de control de ruptura en la barra de herramientas';
        BASinOutput.WindowSize1.Hint :='Cambiar el tamaño de BASin';
        BASinOutput.DisplayWindow1.Hint :='Mostrar u ocultar la ventana de salida de ejecución';
        BASinOutput.SnaptoEditor1.Hint :='Acoplar ventana de visualización a la ventana del editor';
        BASinOutput.N100320x2401.Hint :='Tamaño de ventana normal (1:1)';
        BASinOutput.N200640x4801.Hint :='Tamaño de ventana doble (2:1)';
        BASinOutput.Custom1.Hint :='Las dimensiones actuales';
        BASinOutput.Force11Aspect1.Hint :='Forzar a BASin a mantener la relación de aspecto correcta';
        BASinOutput.ExpressionEvaluator1.Hint :='Mostrar la ventana de calculadora';
        BASinOutput.ProgramInformation1.Hint :='Ver información sobre el programa actual';
        BASinOutput.CommandHistory1.Hint :='Listar el historial de la línea de edición';
        BASinOutput.LastError1.Hint :='Ver el último mensaje de error de ejecución';
        BASinOutput.Find1.Hint :='Buscar texto en el programa actual';
        BASinOutput.Replace1.Hint :='Reemplazar texto en el programa actual';
        BASinOutput.FindNext1.Hint :='Repetir la última operación de búsqueda';
        BASinOutput.ReplaceNext1.Hint :='Repetir la última operación de reemplazo';
        BASinOutput.SourceMarkers1.Hint :='Trabajar con marcadores de código';
        BASinOutput.SetMarker1.Hint :='Establecer marcadores de código';
        BASinOutput.Marker01.Hint :='Establecer marcador de código 0';
        BASinOutput.Marker11.Hint :='Establecer marcador de código 1';
        BASinOutput.Marker21.Hint :='Establecer marcador de código 2';
        BASinOutput.Marker31.Hint :='Establecer marcador de código 3';
        BASinOutput.Marker41.Hint :='Establecer marcador de código 4';
        BASinOutput.Marker51.Hint :='Establecer marcador de código 5';
        BASinOutput.Marker61.Hint :='Establecer marcador de código 6';
        BASinOutput.Marker71.Hint :='Establecer marcador de código 7';
        BASinOutput.Marker81.Hint :='Establecer marcador de código 8';
        BASinOutput.Marker91.Hint :='Establecer marcador de código 9';
        BASinOutput.GetMarker1.Hint :='Saltar a un marcador de código';
        BASinOutput.Marker02.Hint :='Saltar a marcador de código 0';
        BASinOutput.Marker12.Hint :='Saltar a marcador de código 1';
        BASinOutput.Marker22.Hint :='Saltar a marcador de código 2';
        BASinOutput.Marker32.Hint :='Saltar a marcador de código 3';
        BASinOutput.Marker42.Hint :='Saltar a marcador de código 4';
        BASinOutput.Marker52.Hint :='Saltar a marcador de código 5';
        BASinOutput.Marker62.Hint :='Saltar a marcador de código 6';
        BASinOutput.Marker72.Hint :='Saltar a marcador de código 7';
        BASinOutput.Marker82.Hint :='Saltar a marcador de código 8';
        BASinOutput.Marker92.Hint :='Saltar a marcador de código 9';
        BASinOutput.Clearall1.Hint :='Borrar todos los marcadores de código';
        BASinOutput.GotoLineNumber1.Hint :='Establecer el cursor del programa en un número de línea';
        BASinOutput.GotoError1.Hint :='Encontrar la última instrucción con error';
        BASinOutput.Run2.Hint :='Iniciar o detener el programa';
        BASinOutput.GOTO1.Hint :='Ejecutar el programa desde una línea específica';
        BASinOutput.EnableProfiling1.Hint :='Activar sistema de perfilado para cronometrar tu código';
        BASinOutput.ForceBREAK1.Hint :='Restaura el sistema al editor.';
        BASinOutput.TraceExecution1.Hint :='Mostrar visualmente la ejecución en el editor durante el funcionamiento';
        BASinOutput.SingleStepStatement1.Hint :='Recorrer el programa paso a paso';
        BASinOutput.StepToNext1.Hint :='Ejecutar instrucciones hasta alcanzar la siguiente línea';
        BASinOutput.RunTo1.Hint :='Ejecutar líneas hasta alcanzar una línea específica';
        BASinOutput.oggleBreakpoint1.Hint :='Alternar un punto de ruptura en la línea actual';
        BASinOutput.AddBreakpoint1.Hint :='Añadir un punto de ruptura a la lista';
        BASinOutput.AddWatch1.Hint :='Añadir una condición a la lista de inspección';
        BASinOutput.BASinOptions1.Hint :='Configurar BASin';
        BASinOutput.TokenTable1.Hint :='Permite la entrada de Tokens con un solo clic';
        BASinOutput.Renumber1.Hint :='Renumerar el programa actual';
        BASinOutput.ZXPrinterOutput1.Hint :='Mostrar la salida de la ZX Printer emulada';
        BASinOutput.Assembler1.Hint :='Abrir el ensamblador Z80 integrado';
        BASinOutput.Compiler1.Hint :='Compilar el programa actual a texto asm';
        BASinOutput.UDGEditor1.Hint :='Editar gráficamente los UDGs/Caracteres';
        BASinOutput.ScreenPaintbox1.Hint :='Utilidad de pintura de pantalla';
        BASinOutput.TapeCreator1.Hint :='Crear un archivo de cinta (.tap/.tzx/.wav)';
        BASinOutput.BEEPComposer1.Hint :='Crear melodías BEEP visualmente';
        BASinOutput.Contents1.Hint :='Mostrar el archivo de ayuda de BASin';
        BASinOutput.CommandHelp1.Hint :='Obtener ayuda sobre el comando actual';
        BASinOutput.SinclairBASICManual1.Hint :='Mostrar el manual de Sinclair BASIC';
        BASinOutput.ErrorHelp1.Hint :='Obtener ayuda sobre errores de ejecución';
        BASinOutput.About1.Hint :='Acerca de BASin';
        BASinOutput.SourceMarkers2.Hint :='Trabajar con marcadores de código';
        BASinOutput.SetMarker2.Hint :='Establecer marcadores de código';
        BASinOutput.Marker04.Hint :='Establecer marcador de código 0';
        BASinOutput.Marker14.Hint :='Establecer marcador de código 1';
        BASinOutput.Marker24.Hint :='Establecer marcador de código 2';
        BASinOutput.Marker34.Hint :='Establecer marcador de código 3';
        BASinOutput.Marker44.Hint :='Establecer marcador de código 4';
        BASinOutput.Marker54.Hint :='Establecer marcador de código 5';
        BASinOutput.Marker64.Hint :='Establecer marcador de código 6';
        BASinOutput.Marker74.Hint :='Establecer marcador de código 7';
        BASinOutput.Marker84.Hint :='Establecer marcador de código 8';
        BASinOutput.Marker94.Hint :='Establecer marcador de código 9';
        BASinOutput.GetMarker2.Hint :='Saltar a un marcador de código';
        BASinOutput.Marker03.Hint :='Saltar a marcador de código 0';
        BASinOutput.Marker13.Hint :='Saltar a marcador de código 1';
        BASinOutput.Marker23.Hint :='Saltar a marcador de código 2';
        BASinOutput.Marker33.Hint :='Saltar a marcador de código 3';
        BASinOutput.Marker43.Hint :='Saltar a marcador de código 4';
        BASinOutput.Marker53.Hint :='Saltar a marcador de código 5';
        BASinOutput.Marker63.Hint :='Saltar a marcador de código 6';
        BASinOutput.Marker73.Hint :='Saltar a marcador de código 7';
        BASinOutput.Marker83.Hint :='Saltar a marcador de código 8';
        BASinOutput.Marker93.Hint :='Saltar a marcador de código 9';
        BASinOutput.Clearall2.Hint :='Borrar todos los marcadores de código';
        end;

        SetErrors(lang);
end;

end.
 