{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020-2021 Lagunov Aleksey alexs75@yandex.ru

  This library is free software; you can redistribute it and/or modify it
  under the terms of the GNU Library General Public License as published by
  the Free Software Foundation; either version 2 of the License, or (at your
  option) any later version with the following modification:

  As a special exception, the copyright holders of this library give you
  permission to link this library with independent modules to produce an
  executable, regardless of the license terms of these independent modules,and
  to copy and distribute the resulting executable under terms of your choice,
  provided that you also meet, for each linked independent module, the terms
  and conditions of the license of that module. An independent module is a
  module which is not derived from or based on this library. If you modify
  this library, you may extend this exception to your version of the library,
  but you are not obligated to do so. If you do not wish to do so, delete this
  exception statement from your version.

  This program is distributed in the hope that it will be useful, but WITHOUT
  ANY WARRANTY; without even the implied warranty of MERCHANTABILITY or
  FITNESS FOR A PARTICULAR PURPOSE. See the GNU Library General Public License
  for more details.

  You should have received a copy of the GNU Library General Public License
  along with this library; if not, write to the Free Software Foundation,
  Inc., 59 Temple Place - Suite 330, Boston, MA 02111-1307, USA.
}

unit crpt_cmp;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, httpsend, fpJSON, cis_list, CrptGlobalTypes, doc_list,
  receipt_list, crpt_participants_info;

const
//  sAPIURL = 'https://ismp.crpt.ru'; //WORK API

  sAPIURL = 'https://ismp.crpt.ru/api/v3';
  sAPIURL4 = 'https://ismp.crpt.ru/api/v3';

  //тестовый ЦРПТ
//  sAPIURL = 'https://sandbox.crpt.tech/api/v3';
//  sAPIURL4 = 'https://sandbox.crpt.tech/api/v4';
type
  THttpMethod = (hmGET, hmPOST);
  TCRPTProtocolVersion = (crtpVersion3, crtpVersion4);

  { TProxyData }

  TProxyData = class(TPersistent)
  private
    FProxyHost: string;
    FProxyPass: string;
    FProxyPort: string;
    FProxyUser: string;
  published
    procedure Assign(Source: TPersistent); override;
    property ProxyHost:string read FProxyHost write FProxyHost;
    property ProxyPort:string read FProxyPort write FProxyPort;
    property ProxyUser:string read FProxyUser write FProxyUser;
    property ProxyPass:string read FProxyPass write FProxyPass;
  end;

type
  TCRPTComponent = class;
  TOnHttpStatusEnevent = procedure (Sender:TCRPTComponent; AHTTP:THTTPSend) of object;
  TOnSignDataEvent = procedure(Sender:TCRPTComponent; AData:string; out ASign:string) of object;

  { TCRPTComponent }

  TCRPTComponent = class(TComponent)
  private
    FAuthorizationToken:string;
    FAuthorizationTokenTimeStamp:TDateTime;
    FHTTP:THTTPSend;
    FOnHttpStatus: TOnHttpStatusEnevent;
    FOnSignData: TOnSignDataEvent;
    FProxyData: TProxyData;
    FResultCode: integer;
    FResultString: string;
    FResultText: TStrings;
    procedure SetProxyData(AValue: TProxyData);
    function DoAnsverLogin(FUID, FDATA:string):Boolean;
    procedure SaveHttpData(ACmdName: string);
  protected
    function SendCommand(AMethod:THttpMethod; ACommand:string; AParams:string; AData:TStream; AProtocolVersion:TCRPTProtocolVersion):Boolean;
  public
    constructor Create(AOwner: TComponent); override;
    destructor Destroy; override;
    procedure Clear;
    function DoLogin:Boolean;
    function Login:Boolean;
    procedure LoadAuthorizationToken(AFileName:string);

    property ResultText:TStrings read FResultText;
    property ResultCode : integer read FResultCode;
    property ResultString : string read FResultString;


    //2 API аутентификации
    //2.1 API аутентификации
    //2.1.1 Метод получения случайных данных для подписания ЭП
    //2.1.2 Метод получения аутентификационного токена

    //Заказы кодов маркировки
    //Метод создания заказа в ИС МП (Метод устарел)
    function OrderCreate:TJSONObject;
    //Асинхронный метод создания заказа в ИС МП
    function OrderCreateAsinc:TJSONObject;
    //Асинхронный метод создания черновика заказа в ИС МП
    function OrderCreateAsincDraft:TJSONObject;
    //Асинхронный метод обновления черновика заказа в ИС МП
    function OrderUpdateAsincDraft(AOrderDraftID:string):TJSONObject;
    //Асинхронный метод создания заказа из черновика в ИС МП
    function OrderCreateFromDraft(AOrderDraftID:string):TJSONObject;
    //Метод получения статуса заказа в ИС МП
    function OrderGetState(AOrderID: string): TJSONObject;

    //Метод получения списка закзазов
    function GetOrdersList(ALimit: integer=0; AOffset: integer=0): TJSONObject;
    //Метод получения списка шаблонов этикеток участника
    function GetLabelTemplatesList: TJSONObject;
    function GetReceiptList(AFilter: TCrptReceiptListLister): TReceiptItems;
    function GetReceipt(ReceiptId:string): TJSONObject;
    function GetReceiptBody(ReceiptId:string): TReceiptItem;

    //2.1.6. Метод получения информации о товаре по Коду товара
    function GetProductInfo(KMList:TStringArray): TJSONObject;

    //2.1.23. Метод получения списка полученных КМ с возможностью фильтрации
    function CISGetReceivedList(ACis:string): TJSONObject;
    //Запрос информации об участнике оборота товаров по ИНН
    function ClientInfo(AInn: String; AGroup: string = ''): TParticipantsInfo;

    //Коды маркировки и товары
    //Метод получения списка КМ по заданному фильтру с подробной информацией о КМ
    function GetKMList: TJSONObject;
    //Метод получения подробной информации о конкретном КМ
    function GetCISInfo(CIS:string): TJSONObject;
    //3.3 Метод получения списка товаров по заданному фильтру
    function GetGoodsList(CIS: string): TJSONObject;
    //3.4 Получение информации о конкретном маркированном товаре
    function GetGoodInfo: TJSONObject;
    //3.5 Метод получения краткой информации о КМ\списке КМ (общедоступный)
    function GetSimpleGoodsList(KMList:TStringArray): TCISItems;
    function GetSimpleGoodsList(KM:string): TCISItems;
    //Документы

    //Метод получения списка документов, ранее загруженных в ИС МП
    function DocList(AFilter:TCrptDocListFilter): TDocItems;
    //Метод получения содержимого документа, ранее загруженного в ИС МП
    function DocContent(ADocID:string):TDocItem;
    //4.3 Единый метод создания документов
    function DocCreate(const PG, ADocumentFormat, ADocumentType:string;const AProductDocument, ASignature:TStream):string;
    //4.4 Методы создания документов

    //function DocCreateGoodsDescritionFirst(AGroupName:string; AOstDescriptionElement:Tost_description_element):TJSONObject;
    //4.5 Метод создания черновика документа
    //4.6 Метод получения черновика документа
    //4.7 Метод обновления черновика документа
    //4.8 Метод удаления черновика документа
    //4.9 Метод поиска и фильтрации черновика документа
    //5 Справочник товаров GS1/Национального каталога
    //5.1 Метод получения списка GTIN участника оборота товара по ИНН
    //5.2 Метод получения информации о товаре по GTIN
    //6 Справочник кодов ТН ВЭД
    //6.1 Метод получения списка 10-ти значных кодов ТН ВЭД


    //
    property AuthorizationToken:string read FAuthorizationToken write FAuthorizationToken;
    property AuthorizationTokenTimeStamp:TDateTime read FAuthorizationTokenTimeStamp write FAuthorizationTokenTimeStamp;
  published
    property ProxyData:TProxyData read FProxyData write SetProxyData;
    property OnHttpStatus:TOnHttpStatusEnevent read FOnHttpStatus write FOnHttpStatus;
    property OnSignData:TOnSignDataEvent read FOnSignData write FOnSignData;
  end;

procedure AddURLParam(var S:string; AParam, AValue:string); overload;
procedure AddURLParam(var S:string; AParam:string; AValue:integer); overload;
procedure AddURLParam(var S:string; AParam:string); overload;

procedure Register;

implementation
uses jsonparser, sdo_date_utils, crpt_create_doc_data;

{$R crpt.res}

procedure Register;
begin
  RegisterComponents('TradeEquipment',[TCRPTComponent]);
end;

function HTTPEncode(const AStr: String): String;
const
  HTTPAllowed = ['A'..'Z','a'..'z', '*','@','.','_','-', '0'..'9', '$','!','''','(',')'];
var
  SS,S,R: PChar;
  H : String[2];
  L : Integer;
begin
  L:=Length(AStr);
  SetLength(Result,L*3); // Worst case scenario
  if (L=0) then
    exit;
  R:=PChar(Result);
  S:=PChar(AStr);
  SS:=S; // Avoid #0 limit !!
  while ((S-SS)<L) do
  begin
    if S^ in HTTPAllowed then
      R^:=S^
    else
    if (S^=' ') then
      R^:='+'
    else
    begin
      R^:='%';
      H:=HexStr(Ord(S^),2);
      Inc(R);
      R^:=H[1];
      Inc(R);
      R^:=H[2];
    end;
    Inc(R);
    Inc(S);
  end;
  SetLength(Result,R-PChar(Result));
end;

procedure AddURLParam(var S: string; AParam, AValue: string);
begin
  if S<>'' then S:=S + '&';
  if AValue <>'' then
  begin
    //AValue:=StringReplace(AValue, '%', '%25', [rfReplaceAll]);
    S:=S + AParam + '=' + HTTPEncode(AValue)
    //S:=S + AParam + '=' + AValue
  end
  else
    S:=S + AParam
end;

procedure AddURLParam(var S: string; AParam: string; AValue: integer);
begin
  AddURLParam(S, AParam, IntToStr(AValue));
end;

procedure AddURLParam(var S: string; AParam: string);
begin
  AddURLParam(S, AParam, '');
end;

{ TProxyData }

procedure TProxyData.Assign(Source: TPersistent);
begin
  if Source is TProxyData then
  begin
    FProxyHost:=TProxyData(Source).FProxyHost;
    FProxyPass:=TProxyData(Source).FProxyPass;
    FProxyPort:=TProxyData(Source).FProxyPort;
    FProxyUser:=TProxyData(Source).FProxyUser;
  end
  else
  inherited Assign(Source);
end;

{ TCRPTComponent }

procedure TCRPTComponent.SetProxyData(AValue: TProxyData);
begin
  FProxyData.Assign(AValue);
end;

function TCRPTComponent.DoAnsverLogin(FUID, FDATA: string): Boolean;
var
  M: TStringStream;
  MS: TMemoryStream;
  S1: String;
  J, R: TJSONObject;
  P: TJSONParser;
begin
  if not Assigned(FOnSignData) then Exit;

  FOnSignData(Self, FDATA, S1);

  J:=TJSONObject.Create;
  J.Add('uuid', FUID);
  J.Add('data', S1);
  M:=TStringStream.Create(J.FormatJSON);
  M.Position:=0;
  J.Free;


  if SendCommand(hmPOST, '/auth/cert/', '', M, crtpVersion3) then
  begin
    SaveHttpData('dologin_cert');
    FHTTP.Document.Position:=0;
    try
      P:=TJSONParser.Create(FHTTP.Document);
      R:=P.Parse as TJSONObject;
      if FResultCode = 200 then
      begin
        FAuthorizationToken:=JSONStringToString( R.GetPath('token').AsString );
        FAuthorizationTokenTimeStamp:=Now;
      end;
    finally
      P.Free;
      R.Free;
    end;
    FHTTP.Document.Position:=0;
  end;
  M.Free;
end;

procedure TCRPTComponent.SaveHttpData(ACmdName: string);
var
  S: String;
  F: TFileStream;
  P: Int64;
begin
  if ExtractFileExt(ACmdName) = '' then
    ACmdName := ACmdName + '.bin';
  S:=GetTempDir(false) + PathDelim + ACmdName;
  F:=TFileStream.Create(S, fmCreate);
  P:=FHTTP.Document.Position;
  FHTTP.Document.Position:=0;
  FHTTP.Document.SaveToStream(F);
  F.Free;
  FHTTP.Document.Position:=P;
end;

procedure SaveStrData(ACmdName, AData: string);
var
  S: String;
  F: TFileStream;
  P: Int64;
begin
  if ExtractFileExt(ACmdName) = '' then
    ACmdName := ACmdName + '.bin';
  S:=GetTempDir(false) + PathDelim + ACmdName;
  F:=TFileStream.Create(S, fmCreate);
  if AData<>'' then
    F.Write(AData[1], Length(AData));
  F.Free;
end;

function TCRPTComponent.SendCommand(AMethod: THttpMethod; ACommand: string;
  AParams: string; AData: TStream; AProtocolVersion: TCRPTProtocolVersion
  ): Boolean;
var
  S, SMethod, FAPIURL: String;
begin
  //if FApiClientId = '' then
  //  raise EDiadocException.Create(sNotDefindAPIKey);

  FHTTP.Clear;

  if ProxyData.ProxyHost <> '' then
  begin
    FHTTP.ProxyHost:=ProxyData.ProxyHost;
    FHTTP.ProxyPort:=ProxyData.ProxyPort;
    FHTTP.ProxyUser:=ProxyData.ProxyUser;
    FHTTP.ProxyPass:=ProxyData.ProxyPass;
  end
  else
  begin
    FHTTP.ProxyHost:='';
    FHTTP.ProxyPort:='';
    FHTTP.ProxyUser:='';
    FHTTP.ProxyPass:='';
  end;

  FHTTP.KeepAlive:=true;

  if FAuthorizationToken <> '' then
  begin
    S:='Authorization: Bearer' + FAuthorizationToken;
    FHTTP.Headers.Insert(0, S);
  end;

  if AMethod = hmGET then
    SMethod := 'GET'
  else
  begin
    SMethod := 'POST';
   // FHTTP.MimeType := 'application/x-www-form-urlencoded';

    if (not Assigned(AData)) or (AData.Size = 0) then
    begin
      FHTTP.Headers.Insert(0, 'Content-Length: 0');
    end
    else
      FHTTP.Document.LoadFromStream(AData);
  end;

  if AParams <> '' then
    AParams:='?' + AParams;

  FHTTP.MimeType:='application/json';

  if AProtocolVersion = crtpVersion4 then
    FAPIURL:=sAPIURL4
  else
    FAPIURL:=sAPIURL;//crtpVersion3 then

  Result := FHTTP.HTTPMethod(SMethod, FAPIURL + ACommand + AParams);
  FHTTP.Document.Position:=0;
  FResultCode := FHTTP.ResultCode;
  FResultString := FHTTP.ResultString;

  if Assigned(FOnHttpStatus) then
    FOnHttpStatus(Self, FHTTP);

end;

constructor TCRPTComponent.Create(AOwner: TComponent);
begin
  inherited Create(AOwner);

  FHTTP := THTTPSend.Create;
  FHTTP.Protocol:='1.1';

  FProxyData:=TProxyData.Create;
  FResultText:=TStringList.Create;
end;

destructor TCRPTComponent.Destroy;
begin
  FreeAndNil(FResultText);
  FreeAndNil(FProxyData);
  FreeAndNil(FHTTP);
  inherited Destroy;
end;

procedure TCRPTComponent.Clear;
begin
  FAuthorizationToken:='';
  FAuthorizationTokenTimeStamp:=0;
end;

function TCRPTComponent.DoLogin: Boolean;
var
  B: TJSONParser;
  J: TJSONData;
  FDATA, FUID: TJSONStringType;
  R: Int64;
begin
  if (FAuthorizationToken <> '') and (FAuthorizationTokenTimeStamp > (Now - (1 / 20) * 10)) then Exit;
  FAuthorizationToken:='';
  Result:=false;
  if SendCommand(hmGET, '/auth/cert/key', '', nil, crtpVersion3) then
  begin
    if FResultCode = 200 then
    begin
      R:=FHTTP.Document.Size;
      FHTTP.Document.Position:=0;
      B:=TJSONParser.Create(FHTTP.Document);
      J:=B.Parse;
      FUID:=J.GetPath('uuid').AsString;
      FDATA:=J.GetPath('data').AsString;
      J.Free;
      B.Free;

      if FDATA<> '' then
      begin
        Result:=DoAnsverLogin(FUID, FDATA);
      end;
    end;
  end;
end;

function TCRPTComponent.Login: Boolean;
begin
  Clear;
  Result:=DoLogin;
end;

procedure TCRPTComponent.LoadAuthorizationToken(AFileName: string);
var
  F: TFileStream;
begin
  FAuthorizationToken:='';
  if not FileExists(AFileName) then Exit;
  F:=TFileStream.Create(AFileName, fmOpenRead);
  try
    SetLength(FAuthorizationToken, F.Size);
    F.Read(FAuthorizationToken[1], F.Size);
  finally
    F.Free;
  end;
end;

function TCRPTComponent.OrderCreate: TJSONObject;
begin
  //POST /api/v3/facade/order/create
  AbstractError;
end;

function TCRPTComponent.OrderCreateAsinc: TJSONObject;
begin
  //POST /api/v3/facade/order
  AbstractError;
end;

function TCRPTComponent.OrderCreateAsincDraft: TJSONObject;
begin
  //POST /api/v3/facade/order/draft
  AbstractError;
end;

function TCRPTComponent.OrderUpdateAsincDraft(AOrderDraftID: string
  ): TJSONObject;
begin
  //PATCH /api/v3/facade/order/{id черновика заказа}
  AbstractError;
end;

function TCRPTComponent.OrderCreateFromDraft(AOrderDraftID: string
  ): TJSONObject;
begin
  //POST /api/v3/facade/order/{id черновика заказа}
  AbstractError;
end;

function TCRPTComponent.OrderGetState(AOrderID: string): TJSONObject;
begin
  //GET /api/v3/facade/order/{UUID заказа}/details
  AbstractError;
end;

function TCRPTComponent.GetOrdersList(ALimit:integer = 0; AOffset:integer = 0): TJSONObject;
var
  S: String;
  P: TJSONParser;
begin
  //GET /api/v3/facade/order/all
  //GET /api/v3/facade/order/all
  //Параметры запроса:
  //direction - порядок сортировки. Возможные значения "DESC" - по убыванию, "ASC" - по возрастанию. Опциональный параметр.
  //sort - код поля сортировки. Опциональный параметр.
  //limit - максимальное количество заказов в КМ в ответе. Опциональный параметр.
  //offset - номер страницы ответа, начальное значение 0. Опциональный параметр.

  Result:=nil;
  S:='';
  if ALimit > 0 then
    AddURLParam(S, 'limit', ALimit);
  if AOffset > 0 then
    AddURLParam(S, 'offset', ALimit);
  if SendCommand(hmGET, '/facade/order/all', S, nil, crtpVersion3) then
  begin
    FHTTP.Document.Position:=0;
    P:=TJSONParser.Create(FHTTP.Document);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
end;

function TCRPTComponent.GetLabelTemplatesList: TJSONObject;
begin
  //GET /api/v3/facade/identifytools/listV2
  AbstractError;
end;

function TCRPTComponent.GetReceiptList(AFilter: TCrptReceiptListLister
  ): TReceiptItems;
var
  S: String;
//  P: TJSONParser;
begin
  //GET /api/v3/facade/receipt/listV2
  Result:=nil;
  DoLogin;
  S:='';
//  if ACis <> '' then
//    AddURLParam(S, 'cis', ACis);
//  AddURLParam(S, 'cisMatchMode', 'LIKE');

  AddURLParam(S, 'dateFrom', xsd_DateTimeToStr(AFilter.DateFrom, xdkDateTime));
  AddURLParam(S, 'dateTo', xsd_DateTimeToStr(AFilter.DateTo, xdkDateTime));
  if AFilter.Limit > 0 then
    AddURLParam(S, 'limit', AFilter.Limit);
  if SendCommand(hmGET, '/facade/receipt/listV2', S, nil, crtpVersion3) then
  begin
    SaveHttpData('receipt_list');
    FHTTP.Document.Position:=0;
    Result:=TReceiptItems.Create;
    Result.LoadFromStream(FHTTP.Document);
    //P:=TJSONParser.Create(FHTTP.Document);
    //Result:=P.Parse as TJSONObject;
    //P.Free;
  end;
end;

function TCRPTComponent.GetReceipt(ReceiptId: string): TJSONObject;
var
  S: String;
  P: TJSONParser;
begin
  //GET /facade/receipt/{receiptId}
  Result:=nil;
  DoLogin;
  S:='';
//  AddURLParam(S, 'receiptId', ReceiptId);
  if SendCommand(hmGET, '/facade/receipt/'+ReceiptId, S, nil, crtpVersion3) then
  begin
    SaveHttpData('receipt_content');
    //FHTTP.Document.Position:=0;
    //Result:=TReceiptItems.Create;
    //Result.LoadFromStream(FHTTP.Document);
    P:=TJSONParser.Create(FHTTP.Document);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
end;

function TCRPTComponent.GetReceiptBody(ReceiptId: string): TReceiptItem;
var
  S: String;
//  P: TJSONParser;
begin
  //GET /facade/receipt/{receiptId}/body
  Result:=nil;
  DoLogin;
  S:='';
//  AddURLParam(S, 'receiptId', ReceiptId);
  if SendCommand(hmGET, '/facade/receipt/'+ReceiptId+'/body', S, nil, crtpVersion3) then
  begin
    SaveHttpData('receipt_body');
    FHTTP.Document.Position:=0;
    Result:=TReceiptItem.Create;
    Result.LoadFromStream(FHTTP.Document);
    //P:=TJSONParser.Create(FHTTP.Document);
    //Result:=P.Parse as TJSONObject;
    //P.Free;
  end;
end;

function TCRPTComponent.GetProductInfo(KMList: TStringArray): TJSONObject;
var
  S2, S, S1: String;
  P: TJSONParser;
begin
  //  /product/info
  Result:=nil;
  DoLogin;
  S2:='';
  S:='';
  for S1 in KMList do
  begin
    if S2<>'' then S2:=S2 + ',';
    S2:=S2 + S1;
  end;

  AddURLParam(S, 'gtins', S2);

  if SendCommand(hmGET, '/product/info', S, nil, crtpVersion3) then
  begin
    SaveHttpData('product_info');
    FHTTP.Document.Position:=0;
    P:=TJSONParser.Create(FHTTP.Document);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
end;

function TCRPTComponent.CISGetReceivedList(ACis: string): TJSONObject;
var
  S: String;
  P: TJSONParser;
begin
  //GET /api/v3/facade/agent/received/list
  Result:=nil;
  DoLogin;
  S:='';
  if ACis <> '' then
    AddURLParam(S, 'cis', ACis);
  AddURLParam(S, 'cisMatchMode', 'LIKE');
  if SendCommand(hmGET, '/facade/agent/received/list', S, nil, crtpVersion3) then
  begin
    SaveHttpData('received_list');
    FHTTP.Document.Position:=0;
    P:=TJSONParser.Create(FHTTP.Document);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;

end;

function TCRPTComponent.ClientInfo(AInn: String; AGroup: string): TParticipantsInfo;
var
  S, S1: String;
  P: TJSONParser;
  B: Boolean;

function DoCheckError:Boolean;
var
  R: TJSONObject;
  N: TJSONData;
begin
  Result:=false;
  P:=TJSONParser.Create(FHTTP.Document);
  R:=P.Parse as TJSONObject;
  if Assigned(R) then
  begin
    N:=R.Find('code');
    if Assigned(N) then
      Result:=true;
  end;
  P.Free;
end;

begin
  //GET /facade/participants/{inn}
  Result:=nil;
  DoLogin;
  //S:='';
  //AddURLParam(S, 'cis', ACis);

  S:=HTTPEncode(StringReplace(AInn, '%', '%25', [rfReplaceAll]));
  S1:='';
  if AGroup<>'' then
    AddURLParam(S1, 'pg', AGroup);

  if SendCommand(hmGET, '/facade/participants/' + S, S1, nil, crtpVersion3) then
  begin
    SaveHttpData('participants_info');
    FHTTP.Document.Position:=0;

    if (ResultCode = 200)  then;
    begin
      B:=not DoCheckError;
      if B then
      begin
        FHTTP.Document.Position:=0;
        Result:=TParticipantsInfo.Create;
        Result.LoadFromStream(FHTTP.Document);
      end;
    end;
  end;
end;

function TCRPTComponent.GetKMList: TJSONObject;
begin
  //GET /api/v3/facade/identifytools/listV2
  AbstractError;
end;

function TCRPTComponent.GetCISInfo(CIS: string): TJSONObject;
var
  P: TJSONParser;
  S: String;
begin
  //GET /api/v3/facade/identifytools/{cis}
  Result:=nil;
  DoLogin;
  S:=HTTPEncode(StringReplace(CIS, '%', '%25', [rfReplaceAll]));

  if SendCommand(hmGET, '/facade/identifytools/' + S, '', nil, crtpVersion3) then
  begin
    SaveHttpData('cis_info');
    FHTTP.Document.Position:=0;
    P:=TJSONParser.Create(FHTTP.Document);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
end;

//CIS - Строка - Уникальный идентификатор (допускается как полное совпадение, так и частичное)
//emissionDateFrom - Строка - Дата эмиссии, от. Задается в формате yyyy-MM-dd'T'HH:mm:ss.SSS'Z - Пример: 2019-01-01T03:00:00.000Z
//emissionDateTo - Строка - Дата эмиссии, до. Задается в формате yyyy-MM-dd'T'HH:mm:ss.SSS'Z - Пример: 2019-01-10T03:00:00.000Z
//gtin - Строка - Global Trade Item Number
//sn - Строка - Серийный номер
function TCRPTComponent.GetGoodsList(CIS:string): TJSONObject;
var
  S: String;
  P: TJSONParser;
begin
  //GET /api/v3/facade/marked_products/listV2
  Result:=nil;
  S:='';
  AddURLParam(S, 'cis', CIS);
(*  if emissionDateFrom<> '' then
    AddURLParam(S, 'emissionDateFrom', emissionDateFrom);
  if emissionDateTo <> '' then
    AddURLParam(S, 'emissionDateTo', emissionDateTo);
  if gtin<>'' then
    AddURLParam(S, 'gtin', gtin);
  if SN<>'' then
    AddURLParam(S, 'sn', sn);

  tnVed10 Строка 10-значный Код ТН ВЭД ЕАЭС
  producerInn Строка ИНН производителя
  cisStatus Массив строковых
  перечислений Текущий статус товара. Список доступных значений:
  INTRODUCED - введен в оборот
  WAIT_SHIPMENT - ожидает отгрузку (приемку)
  RETIRED - выбыл (розничная реализация)
  LOAN_RETIRED - выбыл (договор рассрочки)
  emissionType
  Строковое перечисление
  Тип эмиссии. Список доступных значений:
  LOCAL - производство РФ
  FOREIGN - ввезен в РФ
  Пагинация/Сортировка
  order
  Строковое перечисление
  Тип сортировки (ascending/descending). Список доступных значений:
  ASC - ascending
  DESC - descending
  uit Строка Значение КМ-"точки отсчета", по которому сортируются записи.
  orderedColumnValue Строка Значение столбца-"точки отсчета", по которому сортируются записи.
  orderColumn Строковое перечисление Название столбца, по которому будет производиться сортировка. Список
  доступных значений:
  emd - дата эмиссии
  pageDir
  Строковое перечисление
  Выбор направления (вперед/назад) для пагинации. Список доступных значений:
  NEXT - вперед
  PREV - назад
  limit
  Тело ответа:
  Целое число
  Максимальное количество записей, которое вернется в качестве ответа.
*)
  if SendCommand(hmGET, '/facade/marked_products/listV2', S, nil, crtpVersion3) then
  begin
    FHTTP.Document.Position:=0;
    P:=TJSONParser.Create(FHTTP.Document);
    Result:=P.Parse as TJSONObject;
    P.Free;
  end;
end;

function TCRPTComponent.GetGoodInfo: TJSONObject;
begin
  //URL: /api/v3/facade/marked_products/{uit}
  //Метод: GET
  AbstractError;
end;

function TCRPTComponent.GetSimpleGoodsList(KMList: TStringArray): TCISItems;
var
  S, CIS: String;
  P: TJSONParser;
  J: TJSONObject;
  JA: TJSONArray;
  M: TStringStream;
begin
  Result:=nil;
  //URL: ?cis={КМ}
  //Метод: GET
  DoLogin;

  J:=TJSONObject.Create;
  JA:=TJSONArray.Create;
  for CIS in KMList do
    JA.Add(CIS);
  J.Add('cises', JA);
  M:=TStringStream.Create(J.FormatJSON);
  M.Position:=0;
  J.Free;
//  M.SaveToFile('/home/alexs/aa.txt');
//  M.Free;

  if SendCommand(hmPOST, '/facade/cis/cis_list', S, M, crtpVersion4) then
  begin
    SaveHttpData('cis_list');
    FHTTP.Document.Position:=0;
    Result:=TCISItems.Create;
    Result.LoadFromStream(FHTTP.Document);
  end
  else
    SaveHttpData('cis_list');
  M.Free;

(*
  S:='';
  for CIS in KMList do
    AddURLParam(S, 'cis', CIS);

  SaveStrData('cis_list_p', S);

  if SendCommand(hmGET, '/facade/cis/cis_list', S, nil) then
  begin
    SaveHttpData('cis_list');
    FHTTP.Document.Position:=0;
    Result:=TCISItems.Create;
    Result.LoadFromStream(FHTTP.Document);
  end;
*)
end;

function TCRPTComponent.GetSimpleGoodsList(KM: string): TCISItems;
begin
  Result:=GetSimpleGoodsList(TStringArray.Create(KM));
end;


function TCRPTComponent.DocList(AFilter: TCrptDocListFilter): TDocItems;
var
  S: String;
  P: TJSONParser;
  DT: TDocumentType;
begin
  //URL: /api/v3/facade/doc/listV2
  //Метод: GET
  //limit=10&order=DESC&\orderColumn=docDate&did=623136d3-7a9b-40c9-8ce3-8091e41f83aa&\
  //orderedColumnValue=2019-01-28T09:30:40.136Z&pageDir=NEXT'
  DoLogin;
  Result:=nil;
  //URL: ?cis={КМ}
  //Метод: GET
  S:='';

  AddURLParam(S, 'dateFrom', xsd_DateTimeToStr(AFilter.DateFrom, xdkDateTime));
  AddURLParam(S, 'dateTo', xsd_DateTimeToStr(AFilter.DateTo, xdkDateTime));
  if AFilter.Limit > 0 then
    AddURLParam(S, 'limit', AFilter.Limit);
  if AFilter.ParticipantInn <> '' then
    AddURLParam(S, 'participantInn', AFilter.ParticipantInn);

  if AFilter.DocumentDirection <> ddAll then
    AddURLParam(S, 'inputFormat', BoolToStr(AFilter.DocumentDirection = ddInput, 'true', 'false'));

  for DT in AFilter.DocumentTypes do
    AddURLParam(S, 'documentType', DocumentTypeStr[DT]);


  if AFilter.DocumentStatus <> UNDEFINED then
    AddURLParam(S, 'documentStatus', DocStatusStr[AFilter.DocumentStatus]);

  //Товарная группа
  //  1  - clothes     – Предметы  одежды,  белье постельное, столовое, туалетное и кухонное;
  //  2  - shoes       – Обувные товары;
  //  3  - tobacco     – Табачная продукция;
  //  4  - perfumery   – Духи и туалетная вода;
  //  5  - tires       – Шины и покрышки пневматические резиновые новые;
  //  6  - electronics – Фотокамеры    (кроме кинокамер), фотовспышки и лампы-вспышки;
  //  7  - pharma      – Лекарственные  препараты  для медицинского применения;
  //  8  - milk        – Молочная продукция;
  //  9  - bicycle     – Велосипеды  и  велосипедные рамы;
  //  10 - wheelchairs – Кресла-коляски
  if SendCommand(hmGET, '/facade/doc/listV2', S, nil, crtpVersion3) then
  begin
    SaveHttpData('doc_list');
    FHTTP.Document.Position:=0;
    Result:=TDocItems.Create;
    Result.LoadFromStream(FHTTP.Document);


{    P:=TJSONParser.Create(FHTTP.Document);
    Result:=P.Parse as TJSONObject;
    P.Free;}
  end;
end;

function TCRPTComponent.DocContent(ADocID: string): TDocItem;
var
  S: String;
  P: TJSONParser;
begin
  //URL:
  //Метод: GET
  DoLogin;
  Result:=nil;
  S:=HTTPEncode(StringReplace(ADocID, '%', '%25', [rfReplaceAll]));
  if SendCommand(hmGET, '/facade/doc/'+S+'/body', '', nil, crtpVersion3) then
  begin
    SaveHttpData('doc_content');
    FHTTP.Document.Position:=0;
    Result:=TDocItem.Create;
    Result.LoadFromStream(FHTTP.Document);
    //P:=TJSONParser.Create(FHTTP.Document);
    //Result:=P.Parse as TJSONObject;
    //P.Free;
  end;
end;

function TCRPTComponent.DocCreate(const PG, ADocumentFormat,
  ADocumentType: string; const AProductDocument, ASignature: TStream): string;
var
  V: TCRPTCreateDocumentData;
  S: String;
  M: TMemoryStream;
begin
  //URL: /api/v3/lk/documents/create
  //Метод: POST
 // AbstractError;
  Result:='';
  DoLogin;

  S:='';
  M:=TMemoryStream.Create;
  AddURLParam(S, 'pg', PG);
  V:=TCRPTCreateDocumentData.Create;
  V.DocumentType:=ADocumentType;
  V.DocumentFormat:=ADocumentFormat;
  V.LoadProductDocument(AProductDocument);
  V.LoadSignature(ASignature);
  V.SaveToStream(M);
  V.Free;
  M.Position:=0;

  if SendCommand(hmPOST, '/lk/documents/create', S, M, crtpVersion3) then
  begin
    SaveHttpData('documents_create');
    FHTTP.Document.Position:=0;
//    if FHTTP.ResultCode = 200 then
    begin
      if FHTTP.Document.Size > 0 then
      begin
        SetLength(Result, FHTTP.Document.Size);
        FHTTP.Document.Position:=0;
        FHTTP.Document.Read(Result[1], FHTTP.Document.Size);
      end;
    end;

{    P:=TJSONParser.Create(FHTTP.Document);
    Result:=P.Parse as TJSONObject;
    P.Free;}
  end;

  M.Free;
  ///lk/documents/create?pg=lpAuthorization:
  //Bearer <ТОКЕН>Content-Type: application/json {     "document_format": "string", "product_document": "<Документ в формате base64>",    "type": "string", "signature": "<Открепленная УКЭП формата Base64>"}
end;
(*
function TCRPTComponent.DocCreateGoodsDescritionFirst(AGroupName: string;
  AOstDescriptionElement: Tost_description_element): TJSONObject;
var
  J: TJSONObject;
  S: String;
begin
  ///api/v3/lk/documents/create
  //Метод: POST
  S:='';
  AddURLParam(S, 'pg', AGroupName);

  J:=TJSONObject.Create;
  J.Add('product_document', '"<Документ в base64>"');
  J.Add('document_format', 'XML');
  J.Add('signature', '<Открепленная подпись в base64>');
  J.Free;
  AbstractError;

end;
*)

end.
