unit Unit1;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, Forms, Controls, Graphics, Dialogs, ComCtrls, StdCtrls,
  ETradeDoc, DividerBevel,
  InvoceExchangeFile, InvoceDocument, InvoiceItem, TransferInfo, Signer,
  AdditionalInfo;

type

  { TForm1 }

  TForm1 = class(TForm)
    Button1: TButton;
    Button2: TButton;
    DividerBevel1: TDividerBevel;
    DividerBevel2: TDividerBevel;
    Edit1: TEdit;
    Edit2: TEdit;
    ETradeDoc1: TETradeDoc;
    Label1: TLabel;
    Label2: TLabel;
    Memo1: TMemo;
    Memo2: TMemo;
    PageControl1: TPageControl;
    TabSheet1: TTabSheet;
    TabSheet2: TTabSheet;
    procedure Button1Click(Sender: TObject);
    procedure Button2Click(Sender: TObject);
    procedure FormCreate(Sender: TObject);
  private
    //invoce
    procedure DumpExchangeFile(D: TExchangeFile; APrefix: string);
    procedure DumpParticipantsInformation(P: TParticipantsInformation; APrefix: string);
    procedure DumpSellerExchangeInformation(P: TSellerExchangeInformation; APrefix: string);
    procedure DumpInvoceDocument(P: TInvoceDocument; APrefix: string);
    procedure DumpInvoiceInformation(P:TInvoiceInformation; APrefix: string);
    procedure DumpInvoiceRevisionInfo(P:TInvoiceRevisionInfo; APrefix: string);
    procedure DumpInvoiceItems(P:TInvoiceItems; APrefix: string);
    procedure DumpInvoiceItemList(P:TInvoiceItemList; APrefix: string);
    procedure DumpInvoiceItem(P:TInvoiceItem; APrefix: string);
    procedure DumpExciseSum(P:TExciseSum; APrefix: string);
    procedure DumpVatSum(P:TVatSum; APrefix: string);
    procedure DumpCustomsDeclarationList(P:TCustomsDeclarationList; APrefix: string);
    procedure DumpCustomsDeclaration(P:TCustomsDeclaration; APrefix: string);
    procedure DumpInvoiceItemAdditional(P:TInvoiceItemAdditional; APrefix: string);
    procedure DumpTraceabilityInformationList(P:TTraceabilityInformationList; APrefix: string);
    procedure DumpTraceabilityInformation(P:TTraceabilityInformation; APrefix: string);
    procedure DumpProductIdentificationNumberList(P:TProductIdentificationNumberList; APrefix: string);
    procedure DumpProductIdentificationNumber(P:TProductIdentificationNumber; APrefix: string);

    procedure DumpTotalForPay(P:TTotalForPay; APrefix: string);
    procedure DumpTransferInfo(P:TTransferInfo; APrefix: string);
    procedure DumpSigners(P:TSignerList; APrefix: string);
    procedure DumpItemsTransferInfo(P:TItemsTransferInfo; APrefix: string);
    procedure DumpAdditionalInfo3(P:TAdditionalInfo3; APrefix: string);
    procedure DumpTextInfoList(P:TTextInfoList; APrefix: string);
    procedure DumpTextInfo(P:TTextInfo; APrefix: string);
    procedure DumpShipmentBaseList(P:TShipmentBaseList; APrefix: string);
    procedure DumpShipmentBase(P:TShipmentBase; APrefix: string);


    procedure WriteLogI(S:string; AParams:array of const);
  public

  end;

var
  Form1: TForm1;

implementation
uses LazFileUtils, LazUTF8;

{$R *.lfm}

{ TForm1 }

procedure TForm1.FormCreate(Sender: TObject);
var
  S: String;
begin
  Memo1.Lines.Clear;

  S:=AppendPathDelim(ExtractFileDir(ParamStr(0))) + AppendPathDelim('data');
  Edit1.Text:=S + 'invoce1.xml';
end;

procedure TForm1.Button1Click(Sender: TObject);
var
  D: TExchangeFile;
begin
  D:=ETradeDoc1.LoadInvoce(Edit1.Text);
  DumpExchangeFile(D, 'СФ');
  D.Free;
end;

procedure TForm1.Button2Click(Sender: TObject);
begin
  //
end;

procedure TForm1.DumpExchangeFile(D: TExchangeFile; APrefix: string);
begin
  if not Assigned(D) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'FileID = %s', [D.FileID]);
  WriteLogI(APrefix + 'FormatVersion = %s', [D.FormatVersion]);
  WriteLogI(APrefix + 'ApplicationVersion = %s', [D.ApplicationVersion]);
  DumpParticipantsInformation(D.ParticipantsInformation, APrefix+ 'ParticipantsInformation');
  DumpInvoceDocument(D.Document, APrefix+ 'Document');
end;

procedure TForm1.DumpParticipantsInformation(P: TParticipantsInformation;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'SenderInfo = %s', [P.SenderInfo]);
  WriteLogI(APrefix + 'RecipientInfo = %s', [P.RecipientInfo]);
  DumpSellerExchangeInformation(P.SellerExchangeInformation, APrefix + 'SellerExchangeInformation');
end;

procedure TForm1.DumpSellerExchangeInformation(P: TSellerExchangeInformation;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'FullName = %s', [P.FullName]);
  WriteLogI(APrefix + 'Inn = %s', [P.Inn]);
  WriteLogI(APrefix + 'IdentifierSenderOperator = %s', [P.IdentifierSenderOperator]);
end;

procedure TForm1.DumpInvoceDocument(P: TInvoceDocument; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'KND = %s', [P.KND]);
  WriteLogI(APrefix + 'DocFunction = %s', [P.DocFunction]);
  WriteLogI(APrefix + 'DocumentNameEL = %s', [P.DocumentNameEL]);
  WriteLogI(APrefix + 'DocumentName = %s', [P.DocumentName]);
  WriteLogI(APrefix + 'InvoceDateCreate = %s', [P.InvoceDateCreate]);
  WriteLogI(APrefix + 'InvoceTimeCreate = %s', [P.InvoceTimeCreate]);
  WriteLogI(APrefix + 'DocumentCreator = %s', [P.DocumentCreator]);
  WriteLogI(APrefix + 'DocumentCreatorBase = %s', [P.DocumentCreatorBase]);
  WriteLogI(APrefix + 'InformationAvailabilityStructureAdditionalInfo = %s', [P.InformationAvailabilityStructureAdditionalInfo]);
  DumpInvoiceInformation(P.InvoiceInformation, APrefix + 'InvoiceInformation');
  DumpInvoiceItems(P.InvoiceItems, APrefix + 'InvoiceItems');
  DumpTransferInfo(P.TransferInfo, APrefix + 'TransferInfo');
  DumpSigners(P.Signers, APrefix + 'Signers');

end;

procedure TForm1.DumpInvoiceInformation(P: TInvoiceInformation; APrefix: string
  );
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'DocumentNumber = %s', [P.DocumentNumber]);
  WriteLogI(APrefix + 'DocumentDate = %s', [P.DocumentDate]);
  WriteLogI(APrefix + 'CurrencyCode = %s', [P.CurrencyCode]);
  DumpInvoiceRevisionInfo(P.InvoiceRevisionInfo, APrefix + 'InvoiceRevisionInfo');
end;

procedure TForm1.DumpInvoiceRevisionInfo(P: TInvoiceRevisionInfo;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'InvoiceRevisionDate = %s', [P.InvoiceRevisionDate]);
  WriteLogI(APrefix + 'InvoiceRevisionNumber = %d', [P.InvoiceRevisionNumber]);
  WriteLogI(APrefix + 'InvoiceRevisionDateDef = %s', [P.InvoiceRevisionDateDef]);
  WriteLogI(APrefix + 'InvoiceRevisionNumberDef = %s', [P.InvoiceRevisionNumberDef]);
end;

procedure TForm1.DumpInvoiceItems(P: TInvoiceItems; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  DumpInvoiceItemList(P.InvoiceItemList, APrefix + 'InvoiceItemList');
  DumpTotalForPay(P.TotalForPay, APrefix + 'TotalForPay');
end;

procedure TForm1.DumpInvoiceItemList(P: TInvoiceItemList; APrefix: string);
var
  i: Integer;
begin
  if not Assigned(P) then Exit;
  for i:=0 to P.Count-1 do
    DumpInvoiceItem(P[i], Format(APrefix + '[%d]', [i]));
end;

procedure TForm1.DumpInvoiceItem(P: TInvoiceItem; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'LineNumber = %d', [P.LineNumber]);
  WriteLogI(APrefix + 'Product = %s', [P.Product]);
  WriteLogI(APrefix + 'UnitCode = %s', [P.UnitCode]);
  WriteLogI(APrefix + 'UnitCodeDef = %s', [P.UnitCodeDef]);
  WriteLogI(APrefix + 'Quantity = %s', [P.Quantity]);
  WriteLogI(APrefix + 'Price = %s', [P.Price]);
  WriteLogI(APrefix + 'SubtotalWithVatExcluded = %s', [P.SubtotalWithVatExcluded]);
  WriteLogI(APrefix + 'TaxRate = %s', [P.TaxRate]);
  WriteLogI(APrefix + 'Subtotal = %s', [P.Subtotal]);
  WriteLogI(APrefix + 'SubtotalDef = %s', [P.SubtotalDef]);
  DumpExciseSum(P.Excise, APrefix + 'Excise');
  DumpVatSum(P.Vat, APrefix + 'Vat');
  DumpCustomsDeclarationList(P.CustomsDeclaration, APrefix + 'CustomsDeclaration');
  DumpInvoiceItemAdditional(P.InvoiceItemAdditional, APrefix + 'InvoiceItemAdditional');
  DumpTextInfoList(P.AdditionalInfo, APrefix + 'AdditionalInfo');
end;

procedure TForm1.DumpExciseSum(P: TExciseSum; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'Sum = %s', [P.Sum]);
  WriteLogI(APrefix + 'Without = %s', [P.Without]);
end;

procedure TForm1.DumpVatSum(P: TVatSum; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'VatValue = %s', [P.VatValue]);
  WriteLogI(APrefix + 'WithoutVat = %s', [P.WithoutVat]);
  WriteLogI(APrefix + 'VatValueDef = %s', [P.VatValueDef]);
end;

procedure TForm1.DumpCustomsDeclarationList(P: TCustomsDeclarationList;
  APrefix: string);
var
  i: Integer;
begin
  if not Assigned(P) then Exit;
  for i:=0 to P.Count-1 do
    DumpCustomsDeclaration(P[i], Format(APrefix + '[%d]', [i]));
end;

procedure TForm1.DumpCustomsDeclaration(P: TCustomsDeclaration; APrefix: string
  );
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'CountryCode = %s', [P.CountryCode]);
  WriteLogI(APrefix + 'CountryCodeDef = %s', [P.CountryCodeDef]);
  WriteLogI(APrefix + 'DeclarationNumber = %s', [P.DeclarationNumber]);
end;

procedure TForm1.DumpInvoiceItemAdditional(P: TInvoiceItemAdditional;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'ItemMark = %s', [P.ItemMark]);
  WriteLogI(APrefix + 'AdditionalProperty = %s', [P.AdditionalProperty]);
  WriteLogI(APrefix + 'UnitName = %s', [P.UnitName]);
  WriteLogI(APrefix + 'CountryNameShort = %s', [P.CountryNameShort]);
  WriteLogI(APrefix + 'ItemToRelease = %s', [P.ItemToRelease]);
  WriteLogI(APrefix + 'ItemCharacteristic = %s', [P.ItemCharacteristic]);
  WriteLogI(APrefix + 'ItemSort = %s', [P.ItemSort]);
  WriteLogI(APrefix + 'ItemArticleNumber = %s', [P.ItemArticleNumber]);
  WriteLogI(APrefix + 'ItemVendorCode = %s', [P.ItemVendorCode]);
  WriteLogI(APrefix + 'ItemCatalogCode = %s', [P.ItemCatalogCode]);
  WriteLogI(APrefix + 'ItemTypeCode = %s', [P.ItemTypeCode]);
  DumpTraceabilityInformationList(P.TraceabilityInformation, APrefix + 'TraceabilityInformation');
  DumpProductIdentificationNumberList(P.ProductIdentificationNumber, APrefix + 'ProductIdentificationNumber');
end;

procedure TForm1.DumpTraceabilityInformationList(
  P: TTraceabilityInformationList; APrefix: string);
var
  i: Integer;
begin
  if not Assigned(P) then Exit;
  for i:=0 to P.Count-1 do
    DumpTraceabilityInformation(P[i], Format(APrefix + '[%d]', [i]));
end;

procedure TForm1.DumpTraceabilityInformation(P: TTraceabilityInformation;
  APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'BatchNumber = %s', [P.BatchNumber]);
  WriteLogI(APrefix + 'UnitCode = %s', [P.UnitCode]);
  WriteLogI(APrefix + 'UnitName = %s', [P.UnitName]);
  WriteLogI(APrefix + 'Quantity = %s', [P.Quantity]);
  WriteLogI(APrefix + 'AdditionalInfo = %s', [P.AdditionalInfo]);
end;

procedure TForm1.DumpProductIdentificationNumberList(
  P: TProductIdentificationNumberList; APrefix: string);
var
  i: Integer;
begin
  if not Assigned(P) then Exit;
  for i:=0 to P.Count-1 do
    DumpProductIdentificationNumber(P[i], Format(APrefix + '[%d]', [i]));
end;

procedure TForm1.DumpProductIdentificationNumber(
  P: TProductIdentificationNumber; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'PackagingIdentificationNumber = %s', [P.PackagingIdentificationNumber]);
  WriteLogI(APrefix + 'CheckMark = %s', [P.CheckMark.Text]);
  WriteLogI(APrefix + 'SecondaryPackagingIdentificationNumber = %s', [P.SecondaryPackagingIdentificationNumber.Text]);
end;

procedure TForm1.DumpTotalForPay(P: TTotalForPay; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'TotalWithVatExcluded = %s', [P.TotalWithVatExcluded]);
  WriteLogI(APrefix + 'Total = %s', [P.Total]);
  WriteLogI(APrefix + 'TotalDef = %s', [P.TotalDef]);
  DumpVatSum(P.Vat, APrefix + 'Vat');
  WriteLogI(APrefix + 'TotalNet = %s', [P.TotalNet]);
end;

procedure TForm1.DumpTransferInfo(P: TTransferInfo; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  DumpItemsTransferInfo(P.ItemsTransferInfo, APrefix + 'ItemsTransferInfo');
  DumpAdditionalInfo3(P.AdditionalInfo3, APrefix + 'AdditionalInfo3');
end;

procedure TForm1.DumpSigners(P: TSignerList; APrefix: string);
begin

end;

procedure TForm1.DumpItemsTransferInfo(P: TItemsTransferInfo; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'OperationInfo = %s', [P.OperationInfo]);
  WriteLogI(APrefix + 'OperationType = %s', [P.OperationType]);
  WriteLogI(APrefix + 'TransferDate = %s', [P.TransferDate]);
  WriteLogI(APrefix + 'DateBegin = %s', [P.DateBegin]);
  WriteLogI(APrefix + 'DateEnd = %s', [P.DateEnd]);
  DumpShipmentBaseList(P.TransferBase, APrefix + 'TransferBase');
  //property TransferEmployee:TTransferEmployee read FTransferEmployee;
  //property TransportationCargo:TTransportationCargo read FTransportationCargo;
  //property CreatedThing:TCreatedThing read FCreatedThing;
end;

procedure TForm1.DumpAdditionalInfo3(P: TAdditionalInfo3; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'InfoFileID = %s', [P.InfoFileID]);
  DumpTextInfoList(P.TextInfo, APrefix + 'TextInfo');
end;

procedure TForm1.DumpTextInfoList(P: TTextInfoList; APrefix: string);
var
  i: Integer;
begin
  if not Assigned(P) then Exit;
  for i:=0 to P.Count-1 do
    DumpTextInfo(P[i], Format(APrefix + '[%d]', [i]));
end;

procedure TForm1.DumpTextInfo(P: TTextInfo; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'ID = %s', [P.ID]);
  WriteLogI(APrefix + 'Value = %s', [P.Value]);
end;

procedure TForm1.DumpShipmentBaseList(P: TShipmentBaseList; APrefix: string);
var
  i: Integer;
begin
  if not Assigned(P) then Exit;
  for i:=0 to P.Count-1 do
    DumpShipmentBase(P[i], Format(APrefix + '[%d]', [i]));
end;

procedure TForm1.DumpShipmentBase(P: TShipmentBase; APrefix: string);
begin
  if not Assigned(P) then Exit;
  APrefix:=APrefix + '.';
  WriteLogI(APrefix + 'BaseDocumentName = %s', [P.BaseDocumentName]);
  WriteLogI(APrefix + 'BaseDocumentNumber = %s', [P.BaseDocumentNumber]);
  WriteLogI(APrefix + 'BaseDocumentDate = %s', [P.BaseDocumentDate]);
  WriteLogI(APrefix + 'BaseDocumentInfo = %s', [P.BaseDocumentInfo]);
  WriteLogI(APrefix + 'BaseDocumentID = %s', [P.BaseDocumentID]);
end;

procedure TForm1.WriteLogI(S: string; AParams: array of const);
begin
  Memo1.Lines.Add(Format(S, AParams));
end;

end.

