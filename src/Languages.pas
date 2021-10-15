unit Languages;

interface
  Uses Windows, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs, Math, ShellAPI, AxCtrls,
  FastIMG, ExtCtrls, StdCtrls, ComCtrls, FastDIB, FastDrawEx, FastFiles, FastSize, FastFX, GraphicEx,
  Utility, ROMUtils,AnimPreview;

  Procedure SetLanguage(Var lang: String);

 var


  ErrorsTR: Array[0..43, 1..4] of AnsiString =
  (('0', 'Baþarýlý', 'Program baþarýyla tamamlandý ya da var olan en büyük sayýlý satýrdan daha büyük bir satýra zýplandý.', 'Herhangi'),
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
   ('L', 'Program BREAK ile kesildi', 'BREAK tuþuna basýldý. Kesme isteði iki ifade arasýnda oluþtu. Satýr ve '+'ifade numarasý BREAK tuþuna basýlmadan önceki ifadeye aittir, fakat CONTINUE sonraki ifadeden devam edecektir (zýplamalarýn gerçekleþmesine izin verecek þekilde), yani ifadeler tekrar edilmez.', 'Herhangi'),
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


implementation

Uses BASINMain;

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
        BASinOutput.Load1.Caption :='Yükle...';
        BASinOutput.ReLOAD1.Caption :='Son Yüklenenler';
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
        BASinOutput.Copy1.Caption :='Kopyala';
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
        BASinOutput.ImportBASIC1.Caption :='Binärdatei importieren...';
        BASinOutput.ImportfromTapeImage1.Caption :='Band-Image anfügen...';
        BASinOutput.N1.Caption :='-';
        BASinOutput.Save1.Caption :='&Speichern';
        BASinOutput.SaveBASICas1.Caption :='Speichern &unter...';
        BASinOutput.N2.Caption :='-';
        BASinOutput.Print1.Caption :='&Drucken...';
        BASinOutput.N9.Caption :='-';
        BASinOutput.Exit1.Caption :='&Beenden';
        BASinOutput.Edit1.Caption :='Edit';
        BASinOutput.Undo1.Caption :='&Rückgängig';
        BASinOutput.Redo1.Caption :='Wiederholen';
        BASinOutput.N6.Caption :='-';
        BASinOutput.Cut1.Caption :='Ausschnei&den';
        BASinOutput.Copy1.Caption :='&Kopieren';
        BASinOutput.Paste1.Caption :='E&infügen';
        BASinOutput.Delete1.Caption :='&Löschen';
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
        end;
end;

end.
 