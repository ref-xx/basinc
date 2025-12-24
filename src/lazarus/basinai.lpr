library BasinAI;

{$mode objfpc}{$H+}

uses
  Classes, SysUtils, fphttpclient, fpjson, jsonparser, opensslsockets;

// 1. MEVCUT FONKSİYON (Değişmedi)
function AskGemini(TargetUrl, ApiKey, UserPrompt, SystemPrompt: PChar): PChar; stdcall;
var
  Client: TFPHttpClient;
  JsonSend, JsonResp: TJSONObject;
  ResponseText, ResultText: String;
  InputData: TStringStream;
  FullUrl: String;
begin
  // ... (Bu fonksiyonun içi öncekiyle aynı, burayı atlıyorum yer kaplamasın diye) ...
  // Eğer elinizdeki kod silindiyse önceki cevaptaki AskGemini kodunu buraya koyun.
  ResultText := 'Error';
  Client := TFPHttpClient.Create(Nil);
  JsonSend := TJSONObject.Create;
  try
    try
      Client.AddHeader('Content-Type', 'application/json');
      JsonSend := TJSONObject(GetJSON('{"contents": [{"parts": [{"text": ""}]}]}'));
      JsonSend.Arrays['contents'].Objects[0].Arrays['parts'].Objects[0].Strings['text'] :=
        StrPas(SystemPrompt) + ' ' + StrPas(UserPrompt);

      InputData := TStringStream.Create(JsonSend.AsJSON);
      try
        Client.RequestBody := InputData;
        FullUrl := StrPas(TargetUrl) + '?key=' + StrPas(ApiKey);
        ResponseText := Client.Post(FullUrl);
      finally
        InputData.Free;
      end;

      if ResponseText <> '' then
      begin
        JsonResp := TJSONObject(GetJSON(ResponseText));
        if Assigned(JsonResp) then
        begin
           try
             if (JsonResp.IndexOfName('candidates') > -1) and (JsonResp.Arrays['candidates'].Count > 0) then
                ResultText := JsonResp.Arrays['candidates'].Objects[0].Objects['content'].Arrays['parts'].Objects[0].Strings['text']
             else
                ResultText := 'AI Error: ' + ResponseText;
           except
             on E: Exception do ResultText := 'JSON Parse Error: ' + E.Message;
           end;
        end;
      end
      else ResultText := 'Empty response.';
    except
      on E: Exception do ResultText := 'HTTP Error: ' + E.Message;
    end;
  finally
    Client.Free;
    JsonSend.Free;
  end;
  Result := StrNew(PChar(ResultText));
end;

// -----------------------------------------------------------------------------
// 2. YENİ FONKSİYON: Model Listesini Getir
// -----------------------------------------------------------------------------
function GetModelList(ApiKey: PChar): PChar; stdcall;
var
  Client: TFPHttpClient;
  ResponseText, ModelListStr: String;
  JsonData: TJSONObject;
  ModelsArray, MethodsArray: TJSONArray;
  ModelObj: TJSONObject;
  i, j: Integer;
  SupportsGenerate: Boolean;
  ModelName: String;
begin
  ModelListStr := '';
  Client := TFPHttpClient.Create(Nil);

  try
    try
      // Google'ın "List Models" endpoint'ine GET isteği atıyoruz
      ResponseText := Client.Get('https://generativelanguage.googleapis.com/v1beta/models?key=' + StrPas(ApiKey));

      JsonData := TJSONObject(GetJSON(ResponseText));
      if Assigned(JsonData) and (JsonData.IndexOfName('models') > -1) then
      begin
        ModelsArray := JsonData.Arrays['models'];

        // Tüm modelleri döngüye al
        for i := 0 to ModelsArray.Count - 1 do
        begin
          ModelObj := TJSONObject(ModelsArray.Objects[i]);
          ModelName := ModelObj.Strings['name']; // Örn: "models/gemini-pro"

          // Bu model "generateContent" destekliyor mu? (Chat modeli mi?)
          SupportsGenerate := False;
          if ModelObj.IndexOfName('supportedGenerationMethods') > -1 then
          begin
            MethodsArray := ModelObj.Arrays['supportedGenerationMethods'];
            for j := 0 to MethodsArray.Count - 1 do
            begin
              if MethodsArray.Strings[j] = 'generateContent' then
              begin
                SupportsGenerate := True;
                Break;
              end;
            end;
          end;

          // Eğer destekliyorsa listeye ekle (Delphi TStringList için alt alta formatta)
          if SupportsGenerate then
          begin
            // Baştaki "models/" takısını temizleyelim, sadece "gemini-pro" kalsın
            if Pos('models/', ModelName) = 1 then
              Delete(ModelName, 1, 7);

            ModelListStr := ModelListStr + ModelName + #13#10;
          end;
        end;
      end;

    except
      on E: Exception do
        ModelListStr := 'Error: ' + E.Message;
    end;
  finally
    Client.Free;
    if Assigned(JsonData) then JsonData.Free;
  end;

  Result := StrNew(PChar(ModelListStr));
end;

exports
  AskGemini,
  GetModelList; // Yeni fonksiyonu dışarı açtık

begin
end.

