unit basinet;

interface

uses
  Windows, idHttp, Messages, SysUtils, Classes, Graphics, Controls, Forms, Dialogs,
  StdCtrls;

type
  TBasinetWindow = class(TForm)
    Memo1: TMemo;
    ListBox1: TListBox;
    Close: TButton;
    Load: TButton;
    Save: TButton;
    Share: TButton;
    Delete: TButton;
    GroupBox1: TGroupBox;
    Label1: TLabel;
    Label2: TLabel;
    BasinetUser: TEdit;
    BasinetPwd: TEdit;
    Button1: TButton;
    procedure FormCreate(Sender: TObject);
  private
    { Private declarations }
  public
    { Public declarations }

    function downloadSrc(var aUrl:ansiString):ansiString;
    Procedure geturl(s:string);
  end;

var
  BasinetWindow: TBasinetWindow;

  const
 INET_USERAGENT    = 'Mozilla/4.0, Indy Library (Windows; en-US)';
 INET_TIMEOUT_SECS = 3;
 INET_REDIRECT_MAX = 10;

implementation

{$R *.DFM}



function TBasinetWindow.downloadSrc(var aUrl: ansiString): ansiString;
begin
 with tIdHttp.create(nil) do begin        //Create Indy http object
  request.userAgent:=INET_USERAGENT;      //Custom user agent string
  redirectMaximum:=INET_REDIRECT_MAX;     //Maximum redirects
  handleRedirects:=INET_REDIRECT_MAX<>0;  //Handle redirects
  readTimeOut:=INET_TIMEOUT_SECS*1000;    //Read timeout msec
  try                                     //Catch errors
   result:=get(aUrl);                     //Do the request
   if url.port='80' then url.port:='';    //Remove port 80 from final URL
   aUrl:=url.getFullURI                   //Return final URL
  except result:='error' end;             //Return an error message if failed
  free                                    //Free the http object
 end

end;

procedure TBasinetWindow.FormCreate(Sender: TObject);
var s:ansiString;
begin

end;

procedure TBasinetWindow.geturl(s: string);
var
http:string;
begin
s:='http://retrojen.org/lib/mag/access.php?show=Ziyaret';
 memo1.lines.text:=downloadSrc(s);
 caption:='URL: '+s
end;

end.
