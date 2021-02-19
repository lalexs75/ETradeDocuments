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
unit receipt_list;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects, AbstractSerializationObjects;

type

  { TDocumentDataDto }

  TDocumentDataDto = class(TJSONSerializationObject)
  private
    FErrors: TXSDStringArray;
    FReceiptId: TXSDStringArray;
    procedure SetErrors(AValue: TXSDStringArray);
    procedure SetReceiptId(AValue: TXSDStringArray);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Errors:TXSDStringArray read FErrors write SetErrors;
    property ReceiptId:TXSDStringArray read FReceiptId write SetReceiptId;
  end;

  { TReceiptContentItem }

  TReceiptContentItem = class(TJSONSerializationObject)
  private
    FName: string;
    FNds: Integer;
    FNdsSum: Integer;
    FPaymentType: Integer;
    FPrice: Integer;
    FProductCode: string;
    FProductType: Integer;
    FQuantity: Double;
    FSum: Integer;
    FUit: string;
    procedure SetName(AValue: string);
    procedure SetNds(AValue: Integer);
    procedure SetNdsSum(AValue: Integer);
    procedure SetPaymentType(AValue: Integer);
    procedure SetPrice(AValue: Integer);
    procedure SetProductCode(AValue: string);
    procedure SetProductType(AValue: Integer);
    procedure SetQuantity(AValue: Double);
    procedure SetSum(AValue: Integer);
    procedure SetUit(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property PaymentType:Integer read FPaymentType write SetPaymentType;
    property ProductType:Integer read FProductType write SetProductType;
    property Name:string read FName write SetName;
    property ProductCode:string read FProductCode write SetProductCode;
    property Price:Integer read FPrice write SetPrice;
    property Quantity:Double read FQuantity write SetQuantity;
    property Nds:Integer read FNds write SetNds;
    property NdsSum:Integer read FNdsSum write SetNdsSum;
    property Sum:Integer read FSum write SetSum;
    property Uit:string read FUit write SetUit;
  end;
  TReceiptContentItems = specialize GXMLSerializationObjectList<TReceiptContentItem>;

  { TReceiptContent }

  TReceiptContent = class(TJSONSerializationObject)
  private
    FBuyerInn: string;
    FCashTotalSum: Integer;
    FCode: Integer;
    FCreditSum: Integer;
    FDateTime: Integer;
    FEcashTotalSum: Integer;
    FFiscalDocumentFormatVer: Integer;
    FFiscalDocumentNumber: Integer;
    FFiscalDriveNumber: string;
    FIndicationfiscalSign: Integer;
    FItems: TReceiptContentItems;
    FkktRegId: string;
    Fnds18: Integer;
    FOfdINN: string;
    FOperationType: Integer;
    FOperatorFIO: string;
    FOperatorInn: string;
    FPrepaidSum: Integer;
    FProvisionSum: Integer;
    FRequestNumber: Integer;
    FRetailAddress: string;
    FRetailPlace: string;
    FShiftNumber: Integer;
    FTaxationType: Integer;
    FTotalSum: Integer;
    FUserInn: string;
    procedure SetBuyerInn(AValue: string);
    procedure SetCashTotalSum(AValue: Integer);
    procedure SetCode(AValue: Integer);
    procedure SetCreditSum(AValue: Integer);
    procedure SetDateTime(AValue: Integer);
    procedure SetEcashTotalSum(AValue: Integer);
    procedure SetFiscalDocumentFormatVer(AValue: Integer);
    procedure SetFiscalDocumentNumber(AValue: Integer);
    procedure SetFiscalDriveNumber(AValue: string);
    procedure SetIndicationfiscalSign(AValue: Integer);
    procedure SetkktRegId(AValue: string);
    procedure Setnds18(AValue: Integer);
    procedure SetOfdINN(AValue: string);
    procedure SetOperationType(AValue: Integer);
    procedure SetOperatorFIO(AValue: string);
    procedure SetOperatorInn(AValue: string);
    procedure SetPrepaidSum(AValue: Integer);
    procedure SetProvisionSum(AValue: Integer);
    procedure SetRequestNumber(AValue: Integer);
    procedure SetRetailAddress(AValue: string);
    procedure SetRetailPlace(AValue: string);
    procedure SetShiftNumber(AValue: Integer);
    procedure SetTaxationType(AValue: Integer);
    procedure SetTotalSum(AValue: Integer);
    procedure SetUserInn(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Code:Integer read FCode write SetCode;
    property IndicationfiscalSign:Integer read FIndicationfiscalSign write SetIndicationfiscalSign;
    property FiscalDocumentFormatVer:Integer read FFiscalDocumentFormatVer write SetFiscalDocumentFormatVer;
    property FiscalDocumentNumber:Integer read FFiscalDocumentNumber write SetFiscalDocumentNumber;
    property FiscalDriveNumber:string read FFiscalDriveNumber write SetFiscalDriveNumber;
    property UserInn:string read FUserInn write SetUserInn;
    property BuyerInn:string read FBuyerInn write SetBuyerInn;
    property RequestNumber:Integer read FRequestNumber write SetRequestNumber;
    property DateTime:Integer read FDateTime write SetDateTime;
    property ShiftNumber:Integer read FShiftNumber write SetShiftNumber;
    property OperationType:Integer read FOperationType write SetOperationType;
    property TaxationType:Integer read FTaxationType write SetTaxationType;
    property OperatorFIO:string read FOperatorFIO write SetOperatorFIO;
    property OperatorInn:string read FOperatorInn write SetOperatorInn;
    property kktRegId:string read FkktRegId write SetkktRegId;
    property RetailAddress:string read FRetailAddress write SetRetailAddress;
    property RetailPlace:string read FRetailPlace write SetRetailPlace;
    property Items:TReceiptContentItems read FItems;
    property TotalSum:Integer read FTotalSum write SetTotalSum;
    property CashTotalSum:Integer read FCashTotalSum write SetCashTotalSum;
    property EcashTotalSum:Integer read FEcashTotalSum write SetEcashTotalSum;
    property PrepaidSum:Integer read FPrepaidSum write SetPrepaidSum;
    property CreditSum:Integer read FCreditSum write SetCreditSum;
    property ProvisionSum:Integer read FProvisionSum write SetProvisionSum;
    property nds18:Integer read Fnds18 write Setnds18;
    property OfdINN:string read FOfdINN write SetOfdINN;
  end;

  { TReceiptBody }

  TReceiptBody = class(TJSONSerializationObject)
  private
    FReceipt: TReceiptContent;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Receipt:TReceiptContent read FReceipt;
  end;
  { TReceiptItem }

  TReceiptItem = class(TJSONSerializationObject)
  private
    FAType: string;
    FBody: TReceiptBody;
    FContent: string;
    FDocDate: string;
    FDocErrors: TXSDStringArray;
    FDocumentDataDto: TDocumentDataDto;
    FDownloadDesc: string;
    FDownloadStatus: string;
    FErrors: TXSDStringArray;
    FInput: Boolean;
    FNumber: string;
    FPdfFile: string;
    FReceivedAt: string;
    FSenderName: string;
    FStatus: string;
    FTotal: Cardinal;
    FVat: Cardinal;
    procedure SetAType(AValue: string);
    procedure SetContent(AValue: string);
    procedure SetDocDate(AValue: string);
    procedure SetDocErrors(AValue: TXSDStringArray);
    procedure SetDownloadDesc(AValue: string);
    procedure SetDownloadStatus(AValue: string);
    procedure SetErrors(AValue: TXSDStringArray);
    procedure SetInput(AValue: Boolean);
    procedure SetNumber(AValue: string);
    procedure SetPdfFile(AValue: string);
    procedure SetReceivedAt(AValue: string);
    procedure SetSenderName(AValue: string);
    procedure SetStatus(AValue: string);
    procedure SetTotal(AValue: Cardinal);
    procedure SetVat(AValue: Cardinal);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Number:string read FNumber write SetNumber;
    property DocDate:string read FDocDate write SetDocDate;
    property ReceivedAt:string read FReceivedAt write SetReceivedAt;
    property AType:string read FAType write SetAType;
    property Status:string read FStatus write SetStatus;
    property SenderName:string read FSenderName write SetSenderName;
    property Total:Cardinal read FTotal write SetTotal;
    property Vat:Cardinal read FVat write SetVat;
    property DownloadStatus:string read FDownloadStatus write SetDownloadStatus;
    property DownloadDesc:string read FDownloadDesc write SetDownloadDesc;
    property Input:Boolean read FInput write SetInput;
    property PdfFile:string read FPdfFile write SetPdfFile;
    property Errors:TXSDStringArray read FErrors write SetErrors;
    property DocErrors:TXSDStringArray read FDocErrors write SetDocErrors;
    property DocumentDataDto:TDocumentDataDto read FDocumentDataDto;
    property Body:TReceiptBody read FBody;
    property Content:string read FContent write SetContent;
  end;
  TReceiptItemList = specialize GXMLSerializationObjectList<TReceiptItem>;

  { TReceiptItems }

  TReceiptItems = class(TJSONSerializationObject)
  private
    FResults: TReceiptItemList;
    FTotal: Integer;
    procedure SetTotal(AValue: Integer);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Results:TReceiptItemList read FResults;
    property Total:Integer read FTotal write SetTotal;
  end;


implementation

{ TReceiptContentItem }

procedure TReceiptContentItem.SetName(AValue: string);
begin
  if FName=AValue then Exit;
  FName:=AValue;
  ModifiedProperty('Name');
end;

procedure TReceiptContentItem.SetNds(AValue: Integer);
begin
  if FNds=AValue then Exit;
  FNds:=AValue;
  ModifiedProperty('Nds');
end;

procedure TReceiptContentItem.SetNdsSum(AValue: Integer);
begin
  if FNdsSum=AValue then Exit;
  FNdsSum:=AValue;
  ModifiedProperty('NdsSum');
end;

procedure TReceiptContentItem.SetPaymentType(AValue: Integer);
begin
  if FPaymentType=AValue then Exit;
  FPaymentType:=AValue;
  ModifiedProperty('PaymentType');
end;

procedure TReceiptContentItem.SetPrice(AValue: Integer);
begin
  if FPrice=AValue then Exit;
  FPrice:=AValue;
  ModifiedProperty('Price');
end;

procedure TReceiptContentItem.SetProductCode(AValue: string);
begin
  if FProductCode=AValue then Exit;
  FProductCode:=AValue;
  ModifiedProperty('ProductCode');
end;

procedure TReceiptContentItem.SetProductType(AValue: Integer);
begin
  if FProductType=AValue then Exit;
  FProductType:=AValue;
  ModifiedProperty('ProductType');
end;

procedure TReceiptContentItem.SetQuantity(AValue: Double);
begin
  if FQuantity=AValue then Exit;
  FQuantity:=AValue;
  ModifiedProperty('Quantity');
end;

procedure TReceiptContentItem.SetSum(AValue: Integer);
begin
  if FSum=AValue then Exit;
  FSum:=AValue;
  ModifiedProperty('Sum');
end;

procedure TReceiptContentItem.SetUit(AValue: string);
begin
  if FUit=AValue then Exit;
  FUit:=AValue;
  ModifiedProperty('Uit');
end;

procedure TReceiptContentItem.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('PaymentType', 'paymentType', [], '', -1, -1);
  RegisterProperty('ProductType', 'productType', [], '', -1, -1);
  RegisterProperty('Name', 'name', [], '', -1, -1);
  RegisterProperty('ProductCode', 'productCode', [], '', -1, -1);
  RegisterProperty('Price', 'price', [], '', -1, -1);
  RegisterProperty('Quantity', 'quantity', [], '', -1, -1);
  RegisterProperty('Nds', 'nds', [], '', -1, -1);
  RegisterProperty('NdsSum', 'ndsSum', [], '', -1, -1);
  RegisterProperty('Sum', 'sum', [], '', -1, -1);
  RegisterProperty('Uit', 'uit', [], '', -1, -1);
end;

procedure TReceiptContentItem.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TReceiptContentItem.Destroy;
begin
  inherited Destroy;
end;

{ TReceiptContent }

procedure TReceiptContent.SetCode(AValue: Integer);
begin
  if FCode=AValue then Exit;
  FCode:=AValue;
  ModifiedProperty('Code');
end;

procedure TReceiptContent.SetCashTotalSum(AValue: Integer);
begin
  if FCashTotalSum=AValue then Exit;
  FCashTotalSum:=AValue;
  ModifiedProperty('CashTotalSum');
end;

procedure TReceiptContent.SetBuyerInn(AValue: string);
begin
  if FBuyerInn=AValue then Exit;
  FBuyerInn:=AValue;
  ModifiedProperty('BuyerInn');
end;

procedure TReceiptContent.SetCreditSum(AValue: Integer);
begin
  if FCreditSum=AValue then Exit;
  FCreditSum:=AValue;
  ModifiedProperty('CreditSum');
end;

procedure TReceiptContent.SetDateTime(AValue: Integer);
begin
  if FDateTime=AValue then Exit;
  FDateTime:=AValue;
  ModifiedProperty('DateTime');
end;

procedure TReceiptContent.SetEcashTotalSum(AValue: Integer);
begin
  if FEcashTotalSum=AValue then Exit;
  FEcashTotalSum:=AValue;
  ModifiedProperty('EcashTotalSum');
end;

procedure TReceiptContent.SetFiscalDocumentFormatVer(AValue: Integer);
begin
  if FFiscalDocumentFormatVer=AValue then Exit;
  FFiscalDocumentFormatVer:=AValue;
  ModifiedProperty('FiscalDocumentFormatVer');
end;

procedure TReceiptContent.SetFiscalDocumentNumber(AValue: Integer);
begin
  if FFiscalDocumentNumber=AValue then Exit;
  FFiscalDocumentNumber:=AValue;
  ModifiedProperty('FiscalDocumentNumber');
end;

procedure TReceiptContent.SetFiscalDriveNumber(AValue: string);
begin
  if FFiscalDriveNumber=AValue then Exit;
  FFiscalDriveNumber:=AValue;
  ModifiedProperty('FiscalDriveNumber');
end;

procedure TReceiptContent.SetIndicationfiscalSign(AValue: Integer);
begin
  if FIndicationfiscalSign=AValue then Exit;
  FIndicationfiscalSign:=AValue;
  ModifiedProperty('IndicationfiscalSign');
end;

procedure TReceiptContent.SetkktRegId(AValue: string);
begin
  if FkktRegId=AValue then Exit;
  FkktRegId:=AValue;
  ModifiedProperty('kktRegId');
end;

procedure TReceiptContent.Setnds18(AValue: Integer);
begin
  if Fnds18=AValue then Exit;
  Fnds18:=AValue;
  ModifiedProperty('nds18');
end;

procedure TReceiptContent.SetOfdINN(AValue: string);
begin
  if FOfdINN=AValue then Exit;
  FOfdINN:=AValue;
  ModifiedProperty('OfdINN');
end;

procedure TReceiptContent.SetOperationType(AValue: Integer);
begin
  if FOperationType=AValue then Exit;
  FOperationType:=AValue;
  ModifiedProperty('OperationType');
end;

procedure TReceiptContent.SetOperatorFIO(AValue: string);
begin
  if FOperatorFIO=AValue then Exit;
  FOperatorFIO:=AValue;
  ModifiedProperty('OperatorFIO');
end;

procedure TReceiptContent.SetOperatorInn(AValue: string);
begin
  if FOperatorInn=AValue then Exit;
  FOperatorInn:=AValue;
  ModifiedProperty('OperatorInn');
end;

procedure TReceiptContent.SetPrepaidSum(AValue: Integer);
begin
  if FPrepaidSum=AValue then Exit;
  FPrepaidSum:=AValue;
  ModifiedProperty('PrepaidSum');
end;

procedure TReceiptContent.SetProvisionSum(AValue: Integer);
begin
  if FProvisionSum=AValue then Exit;
  FProvisionSum:=AValue;
  ModifiedProperty('ProvisionSum');
end;

procedure TReceiptContent.SetRequestNumber(AValue: Integer);
begin
  if FRequestNumber=AValue then Exit;
  FRequestNumber:=AValue;
  ModifiedProperty('RequestNumber');
end;

procedure TReceiptContent.SetRetailAddress(AValue: string);
begin
  if FRetailAddress=AValue then Exit;
  FRetailAddress:=AValue;
  ModifiedProperty('RetailAddress');
end;

procedure TReceiptContent.SetRetailPlace(AValue: string);
begin
  if FRetailPlace=AValue then Exit;
  FRetailPlace:=AValue;
  ModifiedProperty('RetailPlace');
end;

procedure TReceiptContent.SetShiftNumber(AValue: Integer);
begin
  if FShiftNumber=AValue then Exit;
  FShiftNumber:=AValue;
  ModifiedProperty('ShiftNumber');
end;

procedure TReceiptContent.SetTaxationType(AValue: Integer);
begin
  if FTaxationType=AValue then Exit;
  FTaxationType:=AValue;
  ModifiedProperty('TaxationType');
end;

procedure TReceiptContent.SetTotalSum(AValue: Integer);
begin
  if FTotalSum=AValue then Exit;
  FTotalSum:=AValue;
  ModifiedProperty('TotalSum');
end;

procedure TReceiptContent.SetUserInn(AValue: string);
begin
  if FUserInn=AValue then Exit;
  FUserInn:=AValue;
  ModifiedProperty('UserInn');
end;

procedure TReceiptContent.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Code', 'code', [], '', -1, -1);
  RegisterProperty('IndicationfiscalSign', 'indicationfiscalSign', [], '', -1, -1);
  RegisterProperty('FiscalDocumentFormatVer', 'fiscalDocumentFormatVer', [], '', -1, -1);
  RegisterProperty('FiscalDocumentNumber', 'fiscalDocumentNumber', [], '', -1, -1);
  RegisterProperty('FiscalDriveNumber', 'fiscalDriveNumber', [], '', -1, -1);
  RegisterProperty('UserInn', 'userInn', [], '', -1, -1);
  RegisterProperty('RequestNumber', 'requestNumber', [], '', -1, -1);
  RegisterProperty('DateTime', 'dateTime', [], '', -1, -1);
  RegisterProperty('ShiftNumber', 'shiftNumber', [], '', -1, -1);
  RegisterProperty('OperationType', 'operationType', [], '', -1, -1);
  RegisterProperty('TaxationType', 'taxationType', [], '', -1, -1);
  RegisterProperty('OperatorFIO', 'operator', [], '', -1, -1);
  RegisterProperty('OperatorInn', 'operatorInn', [], '', -1, -1);
  RegisterProperty('BuyerInn', 'buyerInn',  [], '', -1, -1);
  RegisterProperty('kktRegId', 'kktRegId', [], '', -1, -1);
  RegisterProperty('RetailAddress', 'retailAddress', [], '', -1, -1);
  RegisterProperty('RetailPlace', 'retailPlace', [], '', -1, -1);
  RegisterProperty('Items', 'items', [], '', -1, -1);
  RegisterProperty('TotalSum', 'totalSum', [], '', -1, -1);
  RegisterProperty('CashTotalSum', 'cashTotalSum', [], '', -1, -1);
  RegisterProperty('EcashTotalSum', 'ecashTotalSum', [], '', -1, -1);
  RegisterProperty('PrepaidSum', 'prepaidSum', [], '', -1, -1);
  RegisterProperty('CreditSum', 'creditSum', [], '', -1, -1);
  RegisterProperty('ProvisionSum', 'provisionSum', [], '', -1, -1);
  RegisterProperty('nds18', 'nds18', [], '', -1, -1);
  RegisterProperty('OfdINN', 'ofdINN', [], '', -1, -1);
end;

procedure TReceiptContent.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FItems:=TReceiptContentItems.Create;
end;

destructor TReceiptContent.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

{ TReceiptBody }

procedure TReceiptBody.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Receipt', 'receipt', [], '', -1, -1);
end;

procedure TReceiptBody.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FReceipt:=TReceiptContent.Create;
end;

destructor TReceiptBody.Destroy;
begin
  FreeAndNil(FReceipt);
  inherited Destroy;
end;

{ TDocumentDataDto }

procedure TDocumentDataDto.SetErrors(AValue: TXSDStringArray);
begin
  if FErrors=AValue then Exit;
  FErrors:=AValue;
  ModifiedProperty('Errors');
end;

procedure TDocumentDataDto.SetReceiptId(AValue: TXSDStringArray);
begin
  if FreceiptId=AValue then Exit;
  FreceiptId:=AValue;
  ModifiedProperty('ReceiptId');
end;

procedure TDocumentDataDto.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Errors', 'errors', [], '', -1, -1);
  RegisterProperty('ReceiptId', 'receiptId', [], '', -1, -1);
end;

procedure TDocumentDataDto.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TDocumentDataDto.Destroy;
begin
  inherited Destroy;
end;

{ TReceiptItem }

procedure TReceiptItem.SetAType(AValue: string);
begin
  if FAType=AValue then Exit;
  FAType:=AValue;
  ModifiedProperty('AType');
end;

procedure TReceiptItem.Setcontent(AValue: string);
begin
  if FContent=AValue then Exit;
  FContent:=AValue;
  ModifiedProperty('Content');
end;

procedure TReceiptItem.SetDocDate(AValue: string);
begin
  if FDocDate=AValue then Exit;
  FDocDate:=AValue;
  ModifiedProperty('DocDate');
end;

procedure TReceiptItem.SetDocErrors(AValue: TXSDStringArray);
begin
  if FDocErrors=AValue then Exit;
  FDocErrors:=AValue;
  ModifiedProperty('DocErrors');
end;

procedure TReceiptItem.SetDownloadDesc(AValue: string);
begin
  if FDownloadDesc=AValue then Exit;
  FDownloadDesc:=AValue;
  ModifiedProperty('');
end;

procedure TReceiptItem.SetDownloadStatus(AValue: string);
begin
  if FDownloadStatus=AValue then Exit;
  FDownloadStatus:=AValue;
  ModifiedProperty('DownloadStatus');
end;

procedure TReceiptItem.SetErrors(AValue: TXSDStringArray);
begin
  if FErrors=AValue then Exit;
  FErrors:=AValue;
  ModifiedProperty('Errors');
end;

procedure TReceiptItem.SetInput(AValue: Boolean);
begin
  if FInput=AValue then Exit;
  FInput:=AValue;
  ModifiedProperty('Input');
end;

procedure TReceiptItem.SetNumber(AValue: string);
begin
  if FNumber=AValue then Exit;
  FNumber:=AValue;
  ModifiedProperty('Number');
end;

procedure TReceiptItem.SetPdfFile(AValue: string);
begin
  if FPdfFile=AValue then Exit;
  FPdfFile:=AValue;
  ModifiedProperty('PdfFile');
end;

procedure TReceiptItem.SetReceivedAt(AValue: string);
begin
  if FReceivedAt=AValue then Exit;
  FReceivedAt:=AValue;
  ModifiedProperty('ReceivedAt');
end;

procedure TReceiptItem.SetSenderName(AValue: string);
begin
  if FSenderName=AValue then Exit;
  FSenderName:=AValue;
  ModifiedProperty('SenderName');
end;

procedure TReceiptItem.SetStatus(AValue: string);
begin
  if FStatus=AValue then Exit;
  FStatus:=AValue;
  ModifiedProperty('Status');
end;

procedure TReceiptItem.SetTotal(AValue: Cardinal);
begin
  if FTotal=AValue then Exit;
  FTotal:=AValue;
  ModifiedProperty('Total');
end;

procedure TReceiptItem.SetVat(AValue: Cardinal);
begin
  if FVat=AValue then Exit;
  FVat:=AValue;
  ModifiedProperty('Vat');
end;

procedure TReceiptItem.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Number', 'number', [], '', -1, -1);
  RegisterProperty('DocDate', 'docDate', [], '', -1, -1);
  RegisterProperty('ReceivedAt', 'receivedAt', [], '', -1, -1);
  RegisterProperty('AType', 'type', [], '', -1, -1);
  RegisterProperty('Status', 'status', [], '', -1, -1);
  RegisterProperty('SenderName', 'senderName', [], '', -1, -1);
  RegisterProperty('Total', 'total', [], '', -1, -1);
  RegisterProperty('Vat', 'vat', [], '', -1, -1);

  RegisterProperty('DownloadStatus', 'downloadStatus', [], '', -1, -1);
  RegisterProperty('DownloadDesc', 'downloadDesc', [], '', -1, -1);
  RegisterProperty('Input', 'input', [], '', -1, -1);
  RegisterProperty('PdfFile', 'pdfFile', [], '', -1, -1);
  RegisterProperty('DocErrors', 'docErrors', [], '', -1, -1);
  RegisterProperty('Errors', 'errors', [], '', -1, -1);
  RegisterProperty('DocumentDataDto', 'documentDataDto', [], '', -1, -1);
  RegisterProperty('Body', 'body', [], '', -1, -1);
  RegisterProperty('Content', 'content', [], '', -1, -1);
end;

procedure TReceiptItem.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FDocumentDataDto:=TDocumentDataDto.Create;
  FBody:=TReceiptBody.Create;
end;

destructor TReceiptItem.Destroy;
begin
  FreeAndNil(FDocumentDataDto);
  FreeAndNil(FBody);
  inherited Destroy;
end;

{ TReceiptItems }

procedure TReceiptItems.SetTotal(AValue: Integer);
begin
  if FTotal=AValue then Exit;
  FTotal:=AValue;
  ModifiedProperty('Total');
end;

procedure TReceiptItems.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Results', 'results', [], '', -1, -1);
  RegisterProperty('Total', 'total', [], '', -1, -1);
end;

procedure TReceiptItems.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FResults:=TReceiptItemList.Create;
end;

destructor TReceiptItems.Destroy;
begin
  FreeAndNil(FResults);
  inherited Destroy;
end;

end.

