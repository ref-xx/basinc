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
  JsonData := nil;

  try
    try
      // Google'?n "List Models" endpoint'ine GET iste?i at?yoruz
      ResponseText := Client.Get('https://generativelanguage.googleapis.com/v1beta/models?key=' + StrPas(ApiKey));

      JsonData := TJSONObject(GetJSON(ResponseText));
      if Assigned(JsonData) and (JsonData.IndexOfName('models') > -1) then
      begin
        ModelsArray := JsonData.Arrays['models'];
        if Assigned(ModelsArray) then
        begin
          // T?m modelleri d?ng?ye al
          for i := 0 to ModelsArray.Count - 1 do
          begin
            ModelObj := TJSONObject(ModelsArray.Objects[i]);
            ModelName := ModelObj.Strings['name']; // ?rn: "models/gemini-pro"

            // Bu model "generateContent" destekliyor mu? (Chat modeli mi?)
            SupportsGenerate := False;
            if ModelObj.IndexOfName('supportedGenerationMethods') > -1 then
            begin
              MethodsArray := ModelObj.Arrays['supportedGenerationMethods'];
              if Assigned(MethodsArray) then
              begin
                for j := 0 to MethodsArray.Count - 1 do
                begin
                  if MethodsArray.Strings[j] = 'generateContent' then
                  begin
                    SupportsGenerate := True;
                    Break;
                  end;
                end;
              end;
            end;

            // E?er destekliyorsa listeye ekle (Delphi TStringList i?in alt alta formatta)
            if SupportsGenerate then
            begin
              // Ba?taki "models/" tak?s?n? temizleyelim, sadece "gemini-pro" kals?n
              if Pos('models/', ModelName) = 1 then
                Delete(ModelName, 1, 7);

              ModelListStr := ModelListStr + ModelName + #13#10;
            end;
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

function GetLocalModels(BaseUrl: PChar): PChar; stdcall;
var
  Client: TFPHttpClient;
  ResponseText, ModelListStr, UrlText, FullUrl: String;
  JsonData: TJSONObject;
  ModelsArray: TJSONArray;
  ModelObj: TJSONObject;
  i: Integer;
begin
  ModelListStr := '';
  Client := TFPHttpClient.Create(Nil);
  JsonData := nil;

  try
    try
      if BaseUrl = nil then
        UrlText := ''
      else
        UrlText := Trim(StrPas(BaseUrl));
      if UrlText = '' then
        raise Exception.Create('Empty URL');

      if (Pos('http://', LowerCase(UrlText)) <> 1) and (Pos('https://', LowerCase(UrlText)) <> 1) then
        UrlText := 'http://' + UrlText;

      if UrlText[Length(UrlText)] <> '/' then
        UrlText := UrlText + '/';

      FullUrl := UrlText + 'api/tags';
      ResponseText := Client.Get(FullUrl);

      JsonData := TJSONObject(GetJSON(ResponseText));
      if Assigned(JsonData) and (JsonData.IndexOfName('models') > -1) then
      begin
        ModelsArray := JsonData.Arrays['models'];
        if Assigned(ModelsArray) then
        begin
          for i := 0 to ModelsArray.Count - 1 do
          begin
            ModelObj := TJSONObject(ModelsArray.Objects[i]);
            if ModelObj.IndexOfName('name') > -1 then
              ModelListStr := ModelListStr + ModelObj.Strings['name'] + #13#10;
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

// -----------------------------------------------------------------------------
// 4. YENI FONKSIYON: Yerel (Ollama) Generate
// -----------------------------------------------------------------------------
function AskLocal(BaseUrl, ModelName, UserPrompt, SystemPrompt: PChar): PChar; stdcall;
var
  Client: TFPHttpClient;
  JsonSend, JsonResp: TJSONObject;
  ResponseText, ResultText, UrlText, FullUrl: String;
  InputData: TStringStream;
  PromptText, SystemText, ModelText: String;
begin
  ResultText := 'Error';
  Client := TFPHttpClient.Create(Nil);
  JsonSend := TJSONObject.Create;
  JsonResp := nil;

  try
    try
      if BaseUrl = nil then
        UrlText := ''
      else
        UrlText := Trim(StrPas(BaseUrl));

      if UrlText = '' then
        raise Exception.Create('Empty URL');

      if (Pos('http://', LowerCase(UrlText)) <> 1) and (Pos('https://', LowerCase(UrlText)) <> 1) then
        UrlText := 'http://' + UrlText;

      if UrlText[Length(UrlText)] <> '/' then
        UrlText := UrlText + '/';

      FullUrl := UrlText + 'api/generate';

      if ModelName = nil then
        ModelText := ''
      else
        ModelText := Trim(StrPas(ModelName));

      if ModelText = '' then
        raise Exception.Create('Empty model');

      if UserPrompt = nil then
        PromptText := ''
      else
        PromptText := StrPas(UserPrompt);

      if SystemPrompt = nil then
        SystemText := ''
      else
        SystemText := StrPas(SystemPrompt);

      Client.AddHeader('Content-Type', 'application/json');
      JsonSend.Add('model', ModelText);
      JsonSend.Add('prompt', PromptText);
      if SystemText <> '' then
        JsonSend.Add('system', SystemText);
      JsonSend.Add('stream', False);

      InputData := TStringStream.Create(JsonSend.AsJSON);
      try
        Client.RequestBody := InputData;
        ResponseText := Client.Post(FullUrl);
      finally
        InputData.Free;
      end;

      if ResponseText <> '' then
      begin
        JsonResp := TJSONObject(GetJSON(ResponseText));
        if Assigned(JsonResp) then
        begin
          if JsonResp.IndexOfName('error') > -1 then
            ResultText := 'Error: ' + JsonResp.Strings['error']
          else if JsonResp.IndexOfName('response') > -1 then
            ResultText := JsonResp.Strings['response']
          else
            ResultText := ResponseText;
        end;
      end
      else
        ResultText := 'Empty response.';
    except
      on E: Exception do ResultText := 'Error: ' + E.Message;
    end;
  finally
    Client.Free;
    JsonSend.Free;
    if Assigned(JsonResp) then JsonResp.Free;
  end;

  Result := StrNew(PChar(ResultText));
end;

exports
  AskGemini,
  GetModelList,
  GetLocalModels,
  AskLocal; // Yeni fonksiyonu disari actik

begin
end.

