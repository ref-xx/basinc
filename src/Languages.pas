unit Languages;

interface
  Uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math, ShellAPI, AxCtrls,
  FastIMG, ExtCtrls, StdCtrls, ComCtrls, FastDIB, FastDrawEx, FastFiles, FastSize, FastFX,
  Utility, ROMUtils,AnimPreview;

  Procedure SetLanguage(Var lang: String);

 var


  ErrorsTR: Array[0..43, 1..4] of AnsiString =
  (('0', 'Ba�ar�l�', 'Program ba�ar�yla tamamland� ya da var olan en b�y�k say�l� sat�rdan daha b�y�k bir sat�ra z�pland�.', 'Herhangi'),
   ('1', 'FOR'#39'u olmayan NEXT', 'Kontrol de�i�keni mevcut de�il (FOR ile kurulumu yap�lmam��), fakat ayn� isimli bir de�i�ken mevcut.', 'NEXT'),
   ('2', 'De�i�ken bulunamad�', 'Bu hata basit de�i�ken i�in s�zkonusu oldu�unda, de�i�kenin FOR, LET, READ ya da INPUT komutlar� taraf�ndan olu�turulmadan ya da diskten okunmadan �nce kullan�lmaya ba�lanm�� oldu�unu g�sterir.'+' Dizi de�i�kenleri i�in ise de�i�ken diskten okunmadan ya da DIM komutu ile kurulumu yap�lmadan kullan�ld���n� g�sterir.', 'Any'),
   ('3', 'Alt �ge bulunamad�', 'Aranan �ge s�ral� de�i�ken boyutlar�n�n d���nda ya da alt �ge say�s� yanl��. E�er alt �ge negatif ise ya da 65535'#39'den b�y�k ise B hatas� verilir.', 'Alt �ge De�i�kenleri, harf dizinleri'),
   ('4', 'Yetersiz bellek', 'Yapmak istenilen i�lem i�in yeterli bellek kalmad�. E�er yorumlay�c� bu durumda tak�l�p kald�ysa, CLEAR komutunu kullanarak bellekte tutulan de�i�kenlerin temizlenmesi gerekmektedir.', 'LET, INPUT, FOR, DIM, GO SUB, LOAD, MERGE. Baz� durumlarda hesaplama an�nda.'),
   ('5', 'Ekran alan� d���nda', 'Bir INPUT komutu 23 sat�rdan fazla metin olu�turmaya �al��t�. Ayr�ca PRINT AT 22,xx kullan�ld���nda da olu�ur.', 'INPUT, PRINT AT'),
   ('6', 'Say� �ok b�y�k ', 'Hesaplamalar sonucunda 10 �zeri 38'#39'den daha b�y�k bir rakam olu�tu.', 'T�m aritmatik hesaplar'),
   ('7', 'GO SUB'#39'u olmayan RETURN', '�al��t�r�lan GO SUB say�s�ndan bir fazla RETURN i�letildi.', 'RETURN'),
   ('8', 'Bilinmeyen hata', 'Yorumlay�c� i�leyemeyece�i bir komut ya da ifade ile kar��la�t�. Bu durumda program ak��� durur.', 'Herhangi bir ifade ya da komut.'),
   ('9', 'STOP komutu', 'CONTINUE komutu STOP'#39'u tekrarlamayacak ve bir sonraki ifadeden devam edecektir.', 'STOP'),
   ('A', 'Ge�ersiz arg�man', 'Fonksiyon i�in verilen arg�man uygun de�il.', 'SQR, LN, ASC, ACS, USR (dizi de�i�keni ile)'),
   ('B', 'Tam say� kapsam d���nda', 'Bir tam say� gerekti�inde, k�s�ratl� bir arg�man en yak�n tam say�ya yuvarlan�r. E�er bu beklenen aral���n d���nda ise bu hata ortaya ��kar. S�ra de�i�ken eri�imi i�in bkz: HATA 3.', 'RUN, RANDOMIZE, POKE, DIM, GO TO, GO SUB, LIST, LLIST, PAUSE, PLOT, CHR$, PEEK, USR (n�merik arg�man ile)'),
   ('C', 'BASIC'#39'de anlam� yok', 'Yaz�lan metin anlaml� bir ifade olu�turmuyor. Ayn� zamanda bir fonksiyon i�in kullan�lan �ok yanl�� bir arg�man hakk�nda da bu hata �retilebilir.', 'VAL, VAL$'),
   ('D', 'BREAK - CONTINUE son komutu tekrarlar', 'Bir �evresel i�lem s�ras�nda BREAK tu�una bas�ld�. Bu raporun ard�ndan kullan�lan CONTINUE komutu ifadeyi tekrarlayacakt�r. L raporu ile kar��la�t�r�n.', 'LOAD, SAVE, VERIFY, MERGE. Ayn� zamanda ekrandaki "Scroll?" sorusuna N, BREAK ya da bo�luk tu�una basarak cevap verdi�inizde.'),
   ('E', 'DATA bitti', 'DATA listesininde bulunan veriler t�kenmesine kar��n program READ ile okumaya devam etmeyi denedi.', 'READ'),
   ('F', 'Ge�ersiz dosya ad�', 'SAVE ile kulland���n�z dosya ad� dosya sisteminizde bir anlam ifade etmiyor.', 'SAVE'),
   ('G', 'Sat�r i�in yer yok', 'Program haf�zas�nda yeni bir sat�r� kaydedecek kadar yer kalmad�.', 'Programa yeni bir sat�r girerken.'),
   ('H', 'INPUTda STOP kullan�ld�', 'INPUT s�ras�nda girilen veri STOP komutu ile ba�l�yor. Hata raporu 9'#39'un aksine, CONTINUE normal davran�r ve INPUT ifadesini tekrar eder.', 'INPUT'),
   ('I', 'NEXT olmadan FOR', 'Hi� tekrar edilmeyecek bir FOR d�ng�s� ayarland� (�rn. FOR n=1 TO 0) ve z�planmas� gereken NEXT komutu bulunamad�.', 'FOR'),
   ('J', 'Ge�ersiz G/� ayg�t�', 'Metin giri� (INPUT) ��k��� (OUT) desteklemeyen bir ayg�ta karakter yazmaya ya da okumaya �al��t�n�z.'+' �rne�in, ekran ak���ndan karakter okumak m�mk�n de�ildir ya da sadece okunabilir bir dosyaya yeni karakterler ekleyemezsiniz. Bu durumda INPUT #2,A$ benzeri bir ifade bu hataya sebep olur.' , 'Ak�� operasyonlar�; OPEN #, CLOSE #, INPUT #, PRINT # vb.'),
   ('K', 'Ge�ersiz Renk', 'Verilen rakam renk i�in ge�ersiz. INK, PAPER ve BORDER komutlar� 0 ile 7 aras�ndaki, BRIGHT, FLASH, INVERSE ve OVER sadece 0, 1, ve 8 rakamlar�n� destekler.', 'INK, PAPER, BORDER, FLASH, BRIGHT, INVERSE, OVER; ayr�ca ayn� i�i yapan kontrol karakterlerinden sonra.'),
   ('L', 'Program BREAK ile kesildi', 'BREAK tu�una bas�ld�. Kesme iste�i iki ifade aras�nda olu�tu. Sat�r ve '+'ifade numaras� BREAK tu�una bas�lmadan �nceki ifadeye aittir, fakat CONTINUE sonraki ifadeden devam edecektir (z�plamalar�n ger�ekle�mesine izin verecek �ekilde), yani ifadeler tekrar edilmez.', 'Herhangi'),
   ('M', 'RAMTOP i�e yaram�yor', 'RAMTOP i�in verilmi� olan adres say�s� �ok b�y�k ya da �ok k���k.', 'CLEAR, bazen RUN'),
   ('N', '�fade kay�p', 'Art�k ge�erli olmayan bir ifadeye z�pland�.', 'RETURN, NEXT, CONTINUE'),
   ('O', 'Ge�ersiz ak��', 'Ge�erli ak�� kanallar� olan 0 ile 15 d���nda bir kanala ya da a��k olmayan bir kanala yaz�lmaya �al���ld�, ya da aral�k d���nda bir kanal a��lmaya �al���ld�.', 'INPUT #, OPEN #, PRINT #'),
   ('P', 'DEF FN olmadan FN', 'Kullan�c� taraf�ndan tan�mlanm�� bir fonksiyon (FN) �nceden tan�mlanmadan (DEF FN) programda kullan�ld�.', 'FN'),
   ('Q', 'Parametre hatas�', 'Yanl�� say�da arg�man ya da arg�manlardan birisinin tipi yanl�� (rakam yerine harf dizisi ya da tersi).', 'Herhangi bir fonksiyon.'),
   ('R', 'Dosya y�kleme hatas�', 'Bir dosya disk ya da kasette bulundu fakat y�klenme s�ras�nda bir hata olu�tu ya da kontrol hatas� verdi.', 'VERIFY, LOAD, MERGE'),
   ('a', 'MERGE hatas�', 'MERGE ! bir sebepten do�ru �al��amad�. Dosya tipi ya da boyutu hatal�.', 'MERGE !'),
   ('b', 'Dosya tipi yanl��', 'Ram disk i�lemi s�ras�nda yanl�� dosya tipi verilmi�. �rne�in LOAD !"name" komutu i�in CODE dosyas� bulunmas�.', 'MERGE !, LOAD !'),
   ('c', 'CODE hatas�', 'Dosya boyutu bellek s�n�rlar�n� a�t�.', 'LOAD! file CODE'),
   ('d', '�ok fazla parantez', 'Arg�manlar�n birinin etraf�nda �ok fazla parantez mevcut.', 'PLAY'),
   ('e', 'Dosya zaten mevcut', 'Verilen dosya ismi zaten bulunuyor.', 'SAVE !'),
   ('f', 'Ge�ersiz isim', 'Dosya ismi yanl�� ya da 10 karakterden fazla.', 'SAVE !, ERASE !'),
   ('g', 'RAMDisk hatas�', 'Bu hata asla g�sterilmeyecektir, RAM hatas�n� g�sterir.', 'LOAD !, SAVE !, CAT !, ERASE !'),
   ('h', 'Dosya bulunamad�', 'RamDiskte verilen dosya ismi bulunmuyor.', 'LOAD !, MERGE !, ERASE !'),
   ('i', 'Ge�ersiz ayg�t', 'FORMAT komutundan sonra verilmi� ayg�t ad� mevcut de�il ya da bir fiziksel ayg�ta ait de�il.', 'FORMAT'),
   ('j', 'Ge�ersiz Baud', 'RS232 ayg�t� i�in baud h�z� s�f�r olarak ayarlanm��.', 'FORMAT LINE'),
   ('k', 'Ge�ersiz nota ad�', 'PLAY komutu tan�mad��� bir harf ile ya da k���k harfli bir karakterle kar��la�t�.', 'PLAY'),
   ('l', 'Say� �ok b�y�k', 'Komut i�in kullan�lan parametrede verilen rakam �ok b�y�k.', 'PLAY'),
   ('m', 'Nota aral�k d���nda', 'Bir dizi diyez ya da bemol notay� ses �ipinin d���na ta��d�.', 'PLAY'),
   ('n', 'Aral�k d���nda', 'Bir parametre �ok b�y�k ya da k���k. E�er hata �ok b�y�kse hata l ortaya ��kar.', 'PLAY'),
   ('o', '�ok fazla ba�l� nota', '�ok fazla nota birbirine ba�lanmaya �al��t�.', 'PLAY'),
   ('?', 'Bilinmeyen hata', 'Bir ifade adres 8'#39'deki rom rutinine z�plamaya sebep oldu fakat ERR NR sistem de�i�keninde ge�ersiz bir rakam mevcuttu.', 'Genellikle BASIC'#39' USR 8 komutu i�letildi�inde olu�ur'));

  ErrorAddressesTR: Array[0..43] Of TSpectrumError =
     ((Address:$1392; Desc:'0 BA�ARILI'; Notify:True),
      (Address:$1394; Desc:'1 FOR'#39'u olmayan NEXT'; Notify:True),
      (Address:$13A4; Desc:'2 De�i�ken bulunamad�'; Notify:True),
      (Address:$13B6; Desc:'3 Alt �ge bulunamad�'; Notify:True),
      (Address:$13C5; Desc:'4 Yetersiz bellek'; Notify:True),
      (Address:$13D2; Desc:'5 Ekran alan� d���nda'; Notify:True),
      (Address:$13DF; Desc:'6 Say� �ok b�y�k'; Notify:True),
      (Address:$13ED; Desc:'7 GO SUB'#39'u olmayan RETURN'; Notify:True),
      (Address:$1401; Desc:'8 Dosya sonu'; Notify:True),
      (Address:$140C; Desc:'9 STOP komutu'; Notify:True),
      (Address:$141A; Desc:'A Ge�ersiz arg�man'; Notify:True),
      (Address:$142A; Desc:'B Tamsay� kapsam d���nda'; Notify:True),
      (Address:$143E; Desc:'C BASIC'#39'de anlam� yok'; Notify:True),
      (Address:$144F; Desc:'D BREAK - CONT son komutu tekrarlar'; Notify:True),
      (Address:$1463; Desc:'E DATA bitti'; Notify:True),
      (Address:$146E; Desc:'F Ge�ersiz dosya ad�'; Notify:True),
      (Address:$147F; Desc:'G Sat�r i�in yer yok'; Notify:True),
      (Address:$148F; Desc:'H INPUTda STOP kullan�ld�'; Notify:True),
      (Address:$149C; Desc:'I NEXT olmadan FOR'; Notify:True),
      (Address:$14AC; Desc:'J Ge�ersiz G/� ayg�t�'; Notify:True),
      (Address:$14BE; Desc:'K Ge�ersiz Renk'; Notify:True),
      (Address:$14CC; Desc:'L Program BREAK ile kesildi'; Notify:True),
      (Address:$14DE; Desc:'M RAMTOP i�e yaram�yor'; Notify:True),
      (Address:$14EC; Desc:'N �fade kay�p'; Notify:True),
      (Address:$14FA; Desc:'O Ge�ersiz ak��'; Notify:True),
      (Address:$1508; Desc:'P DEF FN olmadan FN'; Notify:True),
      (Address:$1516; Desc:'Q Parametre hatas�'; Notify:True),
      (Address:$1525; Desc:'R Dosya y�kleme hatas�'; Notify:True),
      (Address:$1392; Desc:'a MERGE hatas�'; Notify:True),
      (Address:$1392; Desc:'b Yanl�� dosya tipi'; Notify:True),
      (Address:$1392; Desc:'c CODE hatas�'; Notify:True),
      (Address:$1392; Desc:'d �ok fazla parantez'; Notify:True),
      (Address:$1392; Desc:'e Dosya zaten mevcut'; Notify:True),
      (Address:$1392; Desc:'f Ge�ersiz isim'; Notify:True),
      (Address:$1392; Desc:'g RAMDisk hatas�'; Notify:True),
      (Address:$1392; Desc:'h Dosya bulunamad�'; Notify:True),
      (Address:$1392; Desc:'i Ge�ersiz ayg�t'; Notify:True),
      (Address:$1392; Desc:'j Ge�ersiz baud'; Notify:True),
      (Address:$1392; Desc:'k Ge�ersiz nota ad�'; Notify:True),
      (Address:$1392; Desc:'l Say� �ok b�y�k'; Notify:True),
      (Address:$1392; Desc:'m Nota aral�k d���nda'; Notify:True),
      (Address:$1392; Desc:'n Aral�k d���nda'; Notify:True),
      (Address:$1392; Desc:'o �ok fazla ba�l� nota'; Notify:True),
      (Address:$1392; Desc:'? Bilinmeyen hata'; Notify:True));


implementation

Uses BASINMain;

Procedure SetLanguage(Var lang: String);



begin

if (lang='T�rk�e') then
        Begin
        AnimPreviewWindow.Caption :='Canland�rma �n �zlemesi';
        AnimPreviewWindow.Label22.Caption :='G�ncelleme Aral���:';
        AnimPreviewWindow.Label21.Caption :='xx kare';
        AnimPreviewWindow.Label2.Caption :='B�y�kl�k:';
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
        BASinOutput.Load1.Caption :='Y�kle...';
        BASinOutput.ReLOAD1.Caption :='Son Y�klenenler';
        BASinOutput.PreviousSession1.Caption :='�nceki Oturumu A�';
        BASinOutput.N12.Caption :='-';
        BASinOutput.Item11.Caption :='Item1';
        BASinOutput.Item21.Caption :='Item2';
        BASinOutput.Item31.Caption :='Item3';
        BASinOutput.Item41.Caption :='Item4';
        BASinOutput.Item51.Caption :='Item5';
        BASinOutput.Item61.Caption :='Item6';
        BASinOutput.Item71.Caption :='Item7';
        BASinOutput.Item81.Caption :='Item8';
        BASinOutput.ImportBASIC1.Caption :='Binary A�...';
        BASinOutput.ImportfromTapeImage1.Caption :='Teyp �maj� Tak...';
        BASinOutput.N1.Caption :='-';
        BASinOutput.Save1.Caption :='Kaydet';
        BASinOutput.SaveBASICas1.Caption :='Farkl� Kaydet...';
        BASinOutput.N2.Caption :='-';
        BASinOutput.Print1.Caption :='Yazd�r...';
        BASinOutput.N9.Caption :='-';
        BASinOutput.Exit1.Caption :='��k';
        BASinOutput.Edit1.Caption :='D�zen';
        BASinOutput.Undo1.Caption :='Geri Al';
        BASinOutput.Redo1.Caption :='Yinele';
        BASinOutput.N6.Caption :='-';
        BASinOutput.Cut1.Caption :='Kes';
        BASinOutput.Copy1.Caption :='Kopyala';
        BASinOutput.Paste1.Caption :='Yap��t�r';
        BASinOutput.Delete1.Caption :='Sil';
        BASinOutput.N14.Caption :='-';
        BASinOutput.CopyListing1.Caption :='Liste Olarak Kopyala';
        BASinOutput.View1.Caption :='&G�r�n�m';
        BASinOutput.ToolBar1.Caption :='Ara� �ubu�u';
        BASinOutput.StatusBar2.Caption :='Durum �ubu�u';
        BASinOutput.SyntaxHelper1.Caption :='S�zdizim Yard�mc�s�';
        BASinOutput.CharacterRuler1.Caption :='Karakter Cetveli';
        BASinOutput.N15.Caption :='-';
        BASinOutput.ProgramInformation1.Caption :='Program Bilgisi';
        BASinOutput.ZXPrinterOutput1.Caption :='ZX Yaz�c� ��kt�s�';
        BASinOutput.CommandHistory1.Caption :='Komut Ge�mi�i';
        BASinOutput.LastError1.Caption :='Son Hata';
        BASinOutput.DebugWindows1.Caption :='Hata Ay�klama Pencereleri';
        BASinOutput.Variables1.Caption :='De�i�kenler';
        BASinOutput.SystemVariables1.Caption :='Sistem De�i�kenleri';
        BASinOutput.Breakpoints1.Caption :='Kesme Noktalar�';
        BASinOutput.Watches1.Caption :='�zlemeler';
        BASinOutput.GOSUBStack1.Caption :='GO SUB Y���n�';
        BASinOutput.MemoryMap1.Caption :='Bellek Haritas�';
        BASinOutput.MemoryViewer1.Caption :='Bellek Listeleyici';
        BASinOutput.LogWindow1.Caption :='K�t�k Penceresi';
        BASinOutput.ProfileResults1.Caption :='Performans Penceresi';
        BASinOutput.CPUWindow1.Caption :='CPU Penceresi';
        BASinOutput.WindowSize1.Caption :='Sanal Ekran Penceresi';
        BASinOutput.DisplayWindow1.Caption :='G�r�n�r';
        BASinOutput.N100320x2401.Caption :='100% (320x240)';
        BASinOutput.N200640x4801.Caption :='200% (640x480)';
        BASinOutput.Custom1.Caption :='�zel ()';
        BASinOutput.N13.Caption :='-';
        BASinOutput.Force11Aspect1.Caption :='1:1 Oran� Zorla';
        BASinOutput.ExpressionEvaluator1.Caption :='Fonksiyon Hesaplama';
        BASinOutput.Search1.Caption :='Ara';
        BASinOutput.Find1.Caption :='Bul...';
        BASinOutput.Replace1.Caption :='De�i�tir...';
        BASinOutput.FindNext1.Caption :='Sonrakini Bul';
        BASinOutput.ReplaceNext1.Caption :='Sonrakini De�i�tir';
        BASinOutput.N10.Caption :='-';
        BASinOutput.SourceMarkers1.Caption :='Kaynak �mleri';
        BASinOutput.SetMarker1.Caption :='�mle';
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
        BASinOutput.GetMarker1.Caption :='�me Git';
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
        BASinOutput.Clearall1.Caption :='T�m�n� Kald�r';
        BASinOutput.GotoLineNumber1.Caption :='Sat�r numaras�na git...';
        BASinOutput.GotoError1.Caption :='Hataya git';
        BASinOutput.Run1.Caption :='Ba�lat';
        BASinOutput.Run2.Caption :='Ba�lat';
        BASinOutput.Continue1.Caption :='Devam et';
        BASinOutput.GOTO1.Caption :='�mle�e git';
        BASinOutput.EnableProfiling1.Caption :='Performans takibine ba�la';
        BASinOutput.N11.Caption :='-';
        BASinOutput.ForceBREAK1.Caption :='Program� zorla durdur';
        BASinOutput.N7.Caption :='-';
        BASinOutput.TraceExecution1.Caption :='��lem takibi';
        BASinOutput.SingleStepStatement1.Caption :='Tek basamak ilerle';
        BASinOutput.StepToNext1.Caption :='Sonraki basama�a atla';
        BASinOutput.RunTo1.Caption :='�mle�e kadar �al��';
        BASinOutput.N8.Caption :='-';
        BASinOutput.oggleBreakpoint1.Caption :='Kesme noktas� ekle/kald�r';
        BASinOutput.AddBreakpoint1.Caption :='Kesme noktas� ekle...';
        BASinOutput.AddWatch1.Caption :='Takip ekle...';
        BASinOutput.N19.Caption :='-';
        BASinOutput.FullSpeed1.Caption :='Tam H�z Em�lasyon';
        BASinOutput.Tools1.Caption :='Ara�lar';
        BASinOutput.BASinOptions1.Caption :='BasinC Se�enekleri...';
        BASinOutput.N4.Caption :='-';
        BASinOutput.TokenTable1.Caption :='Token Tablosu...';
        BASinOutput.BEEPComposer1.Caption :='BEEP Bestecisi...';
        BASinOutput.Memorygrabber1.Caption :='Bellek Yakalay�c�...';
        BASinOutput.UDGEditor1.Caption :='Grafik Edit�r�...';
        BASinOutput.ScreenPaintbox1.Caption :='Ekran Tuvali...';
        BASinOutput.Renumber1.Caption :='Yeniden Numaraland�r...';
        BASinOutput.TapeCreator1.Caption :='Teyp Kasedi D�zenleyici...';
        BASinOutput.Compiler1.Caption :='Derleyici';
        BASinOutput.Assembler1.Caption :='Assembler';
        BASinOutput.MemoryEditor1.Caption :='Bellek D�zenleyici';
        BASinOutput.Help1.Caption :='Yard�m';
        BASinOutput.Contents1.Caption :='��erik...';
        BASinOutput.CommandHelp1.Caption :='Komut Yard�m�';
        BASinOutput.SinclairBASICManual1.Caption :='Sinclair BASIC Kullanma K�lavuzu';
        BASinOutput.ErrorHelp1.Caption :='Hata Yard�m�...';
        BASinOutput.N3.Caption :='-';
        BASinOutput.About1.Caption :='Hakk�nda...';
        BASinOutput.Token1.Caption :='Token';
        BASinOutput.Help2.Caption :='Token Yard�m�...';
        BASinOutput.Tokenise1.Caption :='Token yap';
        BASinOutput.EditVariable1.Caption :='De�i�ken D�zenle...';
        BASinOutput.FindLine1.Caption :='Sat�r� Bul';
        BASinOutput.StringOperation1.Caption :='Dizi Operasyonu';
        BASinOutput.Wordwrapstring1.Caption :='Sat�r sonu kelime aktar';
        BASinOutput.Splitat32chars1.Caption :='32 Karakterden kes';
        BASinOutput.Insertspaces1.Caption :='Bo�luk ekle';
        BASinOutput.Tokeniseall1.Caption :='T�m�n� token yap';
        BASinOutput.Detokeniseall1.Caption :='T�m�n� metin yap';
        BASinOutput.N16.Caption :='-';
        BASinOutput.Cut2.Caption :='Kes';
        BASinOutput.Copy2.Caption :='Kopyala';
        BASinOutput.Paste2.Caption :='Yap��t�r';
        BASinOutput.N17.Caption :='-';
        BASinOutput.Debug1.Caption :='Debug';
        BASinOutput.ToggleBreakpoint1.Caption :='Kesme noktas� ekle/kald�r';
        BASinOutput.RunToCursor1.Caption :='Sat�ra kadar �al��t�r';
        BASinOutput.GoToCursor1.Caption :='Sat�ra git';
        BASinOutput.WatchVariable1.Caption :='De�i�keni izle';


        //Errors:=ErrorsTR;
        //ErrorAddresses:=ErrorAddressesTR;
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
        BASinOutput.Copy1.Caption :='Copy';
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
        BASinOutput.TokenTable1.Caption :='Token Table...';
        BASinOutput.BEEPComposer1.Caption :='BEEP Composer...';
        BASinOutput.Memorygrabber1.Caption :='Memory grabber...';
        BASinOutput.UDGEditor1.Caption :='UDG Character Editor...';
        BASinOutput.ScreenPaintbox1.Caption :='Image Editor...';
        BASinOutput.Renumber1.Caption :='Renumber...';
        BASinOutput.TapeCreator1.Caption :='Tape Editor...';
        BASinOutput.Compiler1.Caption :='Compiler';
        BASinOutput.Assembler1.Caption :='Assembler';
        BASinOutput.MemoryEditor1.Caption :='Memory Editor...';
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
        end;

if (lang='Deutsch') then
        Begin
                  // urceR14.20110529\BasinMain.dfm
        BASinOutput.Caption :='BASin';
        //BASinOutput.Label1.Caption :='Label1';
        BASinOutput.File1.Caption :='&Datei';
        BASinOutput.New1.Caption :='&Neu';
        BASinOutput.N5.Caption :='-';
        BASinOutput.Load1.Caption :='LOAD ""';
        BASinOutput.ReLOAD1.Caption :='Re-LOAD';
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
        BASinOutput.ImportBASIC1.Caption :='Bin�rdatei importieren...';
        BASinOutput.ImportfromTapeImage1.Caption :='Band-Image anf�gen...';
        BASinOutput.N1.Caption :='-';
        BASinOutput.Save1.Caption :='&Speichern';
        BASinOutput.SaveBASICas1.Caption :='Speichern &unter...';
        BASinOutput.N2.Caption :='-';
        BASinOutput.Print1.Caption :='&Drucken...';
        BASinOutput.N9.Caption :='-';
        BASinOutput.Exit1.Caption :='&Beenden';
        BASinOutput.Edit1.Caption :='Edit';
        BASinOutput.Undo1.Caption :='&R�ckg�ngig';
        BASinOutput.Redo1.Caption :='Wiederholen';
        BASinOutput.N6.Caption :='-';
        BASinOutput.Cut1.Caption :='Ausschnei&den';
        BASinOutput.Copy1.Caption :='&Kopieren';
        BASinOutput.Paste1.Caption :='E&inf�gen';
        BASinOutput.Delete1.Caption :='&L�schen';
        BASinOutput.N14.Caption :='-';
        BASinOutput.CopyListing1.Caption :='Listing kopieren';
        BASinOutput.View1.Caption :='&Ansicht';
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
        BASinOutput.Watches1.Caption :='�berwachung';
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
        BASinOutput.Force11Aspect1.Caption :='1:1-Verh�ltnis';
        BASinOutput.ExpressionEvaluator1.Caption :='Ausdr�cke auswerten';
        BASinOutput.Search1.Caption :='&Suchen';
        BASinOutput.Find1.Caption :='&Suchen...';
        BASinOutput.Replace1.Caption :='&Ersetzen...';
        BASinOutput.FindNext1.Caption :='Weiter Suchen';
        BASinOutput.ReplaceNext1.Caption :='N�chstes ersetzen';
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
        BASinOutput.Clearall1.Caption :='Alle l�schen';
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
        BASinOutput.AddBreakpoint1.Caption :='Haltepunkt hinzuf�gen...';
        BASinOutput.AddWatch1.Caption :='Watch hinzuf�gen...';
        BASinOutput.N19.Caption :='-';
        BASinOutput.FullSpeed1.Caption :='maximale Geschwindigkeit';
        BASinOutput.Tools1.Caption :='Werkzeuge';
        BASinOutput.BASinOptions1.Caption :='Einstellungen...';
        BASinOutput.N4.Caption :='-';
        BASinOutput.TokenTable1.Caption :='Token Tabelle...';
        BASinOutput.BEEPComposer1.Caption :='BEEP Composer...';
        BASinOutput.Memorygrabber1.Caption :='Memory grabber...';
        BASinOutput.UDGEditor1.Caption :='Graphik/Sprite-Editor...';
        BASinOutput.ScreenPaintbox1.Caption :='Screen Paintbox...';
        BASinOutput.Renumber1.Caption :='Neu nummerieren...';
        BASinOutput.TapeCreator1.Caption :='Tape Creator...';
        BASinOutput.Compiler1.Caption :='Compiler';
        BASinOutput.Assembler1.Caption :='Assembler';
        BASinOutput.MemoryEditor1.Caption :='Memory Editor';
        BASinOutput.Help1.Caption :='Hilfe';
        BASinOutput.Contents1.Caption :='Inhalt...';
        BASinOutput.CommandHelp1.Caption :='Hilfe zu Befehlen';
        BASinOutput.SinclairBASICManual1.Caption :='Sinclair BASIC Handbuch';
        BASinOutput.ErrorHelp1.Caption :='Fehlerhilfe...';
        BASinOutput.N3.Caption :='-';
        BASinOutput.About1.Caption :='�ber...';
        BASinOutput.Token1.Caption :='Token';
        BASinOutput.Help2.Caption :='Token-Hilfe...';
        BASinOutput.Tokenise1.Caption :='In Token umwandeln';
        BASinOutput.EditVariable1.Caption :='Variable bearbeiten...';
        BASinOutput.FindLine1.Caption :='Zeile finden';
        BASinOutput.StringOperation1.Caption :='String Operation';
        BASinOutput.Wordwrapstring1.Caption :='Wortumbruch';
        BASinOutput.Splitat32chars1.Caption :='Umbruch nach 32 Zeichen';
        BASinOutput.Insertspaces1.Caption :='Leerzeichen einf�gen';
        BASinOutput.Tokeniseall1.Caption :='alles in Token umwandeln';
        BASinOutput.Detokeniseall1.Caption :='Detokenise all';
        BASinOutput.N16.Caption :='-';
        BASinOutput.Cut2.Caption :='Ausschneiden';
        BASinOutput.Copy2.Caption :='Kopieren';
        BASinOutput.Paste2.Caption :='Einf�gen';
        BASinOutput.N17.Caption :='-';
        BASinOutput.Debug1.Caption :='Debug';
        BASinOutput.ToggleBreakpoint1.Caption :='Haltepunkt umschalten';
        BASinOutput.RunToCursor1.Caption :='Laufe bis Zeile';
        BASinOutput.GoToCursor1.Caption :='Gehe zu Zeile';
        BASinOutput.WatchVariable1.Caption :='�berwache Variable';
        end;


if (lang='Spanish') then
        Begin
                  // urceR14.20110529\BasinMain.dfm
        BASinOutput.Caption :=ReleaseName;
        //BASinOutput.Label1.Caption :='Label1';
        BASinOutput.File1.Caption :='&Archivo';
        BASinOutput.New1.Caption :='Nuevo';
        BASinOutput.N5.Caption :='-';
        BASinOutput.Load1.Caption :='LOAD ""';
        BASinOutput.ReLOAD1.Caption :='Re-LOAD';
        BASinOutput.PreviousSession1.Caption :='Sesi�n anterior';
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
        BASinOutput.Copy1.Caption :='Copiar';
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
        BASinOutput.ProgramInformation1.Caption :='Informaci�n del Programa';
        BASinOutput.ZXPrinterOutput1.Caption :='Salida Impresora ZX';
        BASinOutput.CommandHistory1.Caption :='Hist�rico de Comandos';
        BASinOutput.LastError1.Caption :='�ltimo Error';
        BASinOutput.DebugWindows1.Caption :='Ventanas Depuraci�n';
        BASinOutput.Variables1.Caption :='Variables';
        BASinOutput.SystemVariables1.Caption :='Variables de Sistema';
        BASinOutput.Breakpoints1.Caption :='Puntos Parada';
        BASinOutput.Watches1.Caption :='Puntos Observaci�n';
        BASinOutput.GOSUBStack1.Caption :='Pila GO SUB';
        BASinOutput.MemoryMap1.Caption :='Mapa Memoria';
        BASinOutput.MemoryViewer1.Caption :='Visor Memoria';
        BASinOutput.LogWindow1.Caption :='Ventana Registro';
        BASinOutput.ProfileResults1.Caption :='Resultados Perfilador';
        BASinOutput.CPUWindow1.Caption :='Ventana CPU';
        BASinOutput.WindowSize1.Caption :='Ventana Visualizaci�n';
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
        BASinOutput.SourceMarkers1.Caption :='Marcadores de C�digo';
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
        BASinOutput.GotoLineNumber1.Caption :='Ir a L�nea...';
        BASinOutput.GotoError1.Caption :='Ir a Error';
        BASinOutput.Run1.Caption :='Ejecutar';
        BASinOutput.Run2.Caption :='Ejecutar';
        BASinOutput.Continue1.Caption :='Continuar';
        BASinOutput.GOTO1.Caption :='Ir al Cursor';
        BASinOutput.EnableProfiling1.Caption :='Habilitar Perfilado';
        BASinOutput.N11.Caption :='-';
        BASinOutput.ForceBREAK1.Caption :='Forzar BREAK';
        BASinOutput.N7.Caption :='-';
        BASinOutput.TraceExecution1.Caption :='Trazar Ejecuci�n';
        BASinOutput.SingleStepStatement1.Caption :='Avanzar Instrucci�n';
        BASinOutput.StepToNext1.Caption :='Volver Instrucci�n';
        BASinOutput.RunTo1.Caption :='Ejecutar Hasta Cursor';
        BASinOutput.N8.Caption :='-';
        BASinOutput.oggleBreakpoint1.Caption :='Cambiar Punto Ruptura';
        BASinOutput.AddBreakpoint1.Caption :='A�adir Punto Ruptura...';
        BASinOutput.AddWatch1.Caption :='A�adir Punto Observaci�n...';
        BASinOutput.N19.Caption :='-';
        BASinOutput.FullSpeed1.Caption :='Emulaci�n a Toda Velocidad';
        BASinOutput.Tools1.Caption :='Herramientas';
        BASinOutput.BASinOptions1.Caption :='Opciones BASin...';
        BASinOutput.N4.Caption :='-';
        BASinOutput.TokenTable1.Caption :='Tabla de Token...';
        BASinOutput.BEEPComposer1.Caption :='Compositor BEEP...';
        BASinOutput.Memorygrabber1.Caption :='Capturador Memoria...';
        BASinOutput.UDGEditor1.Caption :='Editor Gr�fico...';
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
        BASinOutput.FindLine1.Caption :='Buscar L�nea';
        BASinOutput.StringOperation1.Caption :='Operaciones de Cadenas';
        BASinOutput.Wordwrapstring1.Caption :='Ajuste L�nea';
        BASinOutput.Splitat32chars1.Caption :='Recortar a los 32 car�cteres';
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
        BASinOutput.RunToCursor1.Caption :='Ejecutar Hasta L�nea';
        BASinOutput.GoToCursor1.Caption :='Ir a L�nea';
        BASinOutput.WatchVariable1.Caption :='Observar Variable';
        end;
end;

end.
 