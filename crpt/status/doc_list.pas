{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020 Lagunov Aleksey alexs75@yandex.ru

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

unit doc_list;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects, AbstractSerializationObjects;

type

  { TDocumentDataDto }

  TDocumentDataDto =  class(TJSONSerializationObject)
  private
    FDid: string;
    FEdoRecipient: string;
    FElr: Integer;
    FErrors: string;
    Fk_offset: Integer;
    Fk_partition: Integer;
    FOst: string;
    FReceiptId: TXSDStringArray;
    FSigningDate: Int64;
    FSignRecipientFio: string;
    FT: Integer;
    FTrn: Integer;
    procedure SetDid(AValue: string);
    procedure SetEdoRecipient(AValue: string);
    procedure SetElr(AValue: Integer);
    procedure SetErrors(AValue: string);
    procedure Setk_offset(AValue: Integer);
    procedure Setk_partition(AValue: Integer);
    procedure SetOst(AValue: string);
    procedure SetReceiptId(AValue: TXSDStringArray);
    procedure SetSigningDate(AValue: Int64);
    procedure SetSignRecipientFio(AValue: string);
    procedure SetT(AValue: Integer);
    procedure SetTrn(AValue: Integer);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Ost:string read FOst write SetOst;
    property Trn:Integer read FTrn write SetTrn;
    property Errors:string read FErrors write SetErrors;
    property SigningDate : Int64 read FSigningDate write SetSigningDate;
    property SignRecipientFio:string read FSignRecipientFio write SetSignRecipientFio;
    property EdoRecipient:string read FEdoRecipient write SetEdoRecipient;
    property Did:string read FDid write SetDid;
    property T:Integer read FT write SetT;
    property k_offset:Integer read Fk_offset write Setk_offset;
    property k_partition:Integer read Fk_partition write Setk_partition;
    property Elr:Integer read FElr write SetElr;
    property ReceiptId:TXSDStringArray read FReceiptId write SetReceiptId;
  end;

  { TDocItem }

  TDocItem = class(TJSONSerializationObject)
  private
    FAType: string;
    FBody: string;
    FcisTotal: Integer;
    FContent: string;
    FDocDate: string;
    FDocErrors: string;
    FDocumentDataDto: TDocumentDataDto;
    FDownloadDesc: string;
    FDownloadStatus: string;
    FErrors: TXSDStringArray;
    FInput: Boolean;
    FInvoiceDate: string;
    FInvoiceNumber: string;
    FNumber: string;
    FPdfFile: string;
    FReceivedAt: string;
    FReceiverName: string;
    FSenderName: string;
    FStatus: string;
    FTotal: Integer;
    FVat: Integer;
    function GetDocumentDate: TDateTime;
    function GetDocumentInvoiceDate: TDateTime;
    function GetDocumentReceivedAt: TDateTime;
    procedure SetAType(AValue: string);
    procedure SetBody(AValue: string);
    procedure SetcisTotal(AValue: Integer);
    procedure SetContent(AValue: string);
    procedure SetDocDate(AValue: string);
    procedure SetDocErrors(AValue: string);
    procedure SetDocumentDate(AValue: TDateTime);
    procedure SetDocumentInvoiceDate(AValue: TDateTime);
    procedure SetDocumentReceivedAt(AValue: TDateTime);
    procedure SetDownloadDesc(AValue: string);
    procedure SetDownloadStatus(AValue: string);
    procedure SetErrors(AValue: TXSDStringArray);
    procedure SetInput(AValue: Boolean);
    procedure SetInvoiceDate(AValue: string);
    procedure SetInvoiceNumber(AValue: string);
    procedure SetNumber(AValue: string);
    procedure SetPdfFile(AValue: string);
    procedure SetReceivedAt(AValue: string);
    procedure SetReceiverName(AValue: string);
    procedure SetSenderName(AValue: string);
    procedure SetStatus(AValue: string);
    procedure SetTotal(AValue: Integer);
    procedure SetVat(AValue: Integer);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
    property DocumentDate:TDateTime read GetDocumentDate write SetDocumentDate;
    property DocumentReceivedAt:TDateTime read GetDocumentReceivedAt write SetDocumentReceivedAt;
    property DocumentInvoiceDate:TDateTime read GetDocumentInvoiceDate write SetDocumentInvoiceDate;
  published
    property Number:string read FNumber write SetNumber;
    property DocDate:string read FDocDate write SetDocDate;
    property ReceivedAt:string read FReceivedAt write SetReceivedAt;
    property AType:string read FAType write SetAType;
    property Status:string read FStatus write SetStatus;
    //externalId
    property SenderName:string read FSenderName write SetSenderName;
    property ReceiverName:string read FReceiverName write SetReceiverName;
    property InvoiceNumber:string read FInvoiceNumber write SetInvoiceNumber;
    property InvoiceDate:string read FInvoiceDate write SetInvoiceDate;
    property Total:Integer read FTotal write SetTotal;
    property Vat:Integer read FVat write SetVat;
    property DownloadStatus:string read FDownloadStatus write SetDownloadStatus;
    property DownloadDesc:string read FDownloadDesc write SetDownloadDesc;
    property cisTotal:Integer read FcisTotal write SetcisTotal;
    property Body:string read FBody write SetBody;
    property Content:string read FContent write SetContent;
    property Input:Boolean read FInput write SetInput;
    property DocErrors:string read FDocErrors write SetDocErrors;
    property PdfFile:string read FPdfFile write SetPdfFile;
    property Errors:TXSDStringArray read FErrors write SetErrors;
    //atk
    //sender
    property DocumentDataDto:TDocumentDataDto read FDocumentDataDto;
  end;
  TDocItemList = specialize GXMLSerializationObjectList<TDocItem>;

  { TDocItems }

  TDocItems = class(TJSONSerializationObject)
  private
    FResults: TDocItemList;
    FTotal: Integer;
    procedure SetTotal(AValue: Integer);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Results:TDocItemList read FResults;
    property Total:Integer read FTotal write SetTotal;
  end;

implementation
uses sdo_date_utils, DateUtils;

{ TDocItems }

procedure TDocItems.SetTotal(AValue: Integer);
begin
  if FTotal=AValue then Exit;
  FTotal:=AValue;
  ModifiedProperty('Total');
end;

procedure TDocItems.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Results', 'results', [], '', -1, -1);
  RegisterProperty('Total', 'total', [], '', -1, -1);
end;

procedure TDocItems.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FResults:=TDocItemList.Create;
end;

destructor TDocItems.Destroy;
begin
  FreeAndNil(FResults);
  inherited Destroy;
end;

{ TDocItem }

procedure TDocItem.SetAType(AValue: string);
begin
  if FAType=AValue then Exit;
  FAType:=AValue;
  ModifiedProperty('AType');
end;

function TDocItem.GetDocumentDate: TDateTime;
var
  R: TDateTimeRec;
begin
  if xsd_TryStrToDate(FDocDate, R, xdkDateTime) then
    Result:=NormalizeToUTC(R);
end;

function TDocItem.GetDocumentInvoiceDate: TDateTime;
var
  R: TDateTimeRec;
begin
//  Result:=UnixToDateTime(FInvoiceDate div 1000);
  if xsd_TryStrToDate(FDocDate, R, xdkDateTime) then
    Result:=NormalizeToUTC(R);
end;

function TDocItem.GetDocumentReceivedAt: TDateTime;
var
  R: TDateTimeRec;
begin
  if xsd_TryStrToDate(FReceivedAt, R, xdkDateTime) then
    Result:=NormalizeToUTC(R);
end;

procedure TDocItem.SetBody(AValue: string);
begin
  if FBody=AValue then Exit;
  FBody:=AValue;
  ModifiedProperty('Body');
end;

procedure TDocItem.SetcisTotal(AValue: Integer);
begin
  if FcisTotal=AValue then Exit;
  FcisTotal:=AValue;
  ModifiedProperty('cisTotal');
end;

procedure TDocItem.SetContent(AValue: string);
begin
  if FContent=AValue then Exit;
  FContent:=AValue;
  ModifiedProperty('Content');
end;

procedure TDocItem.SetDocDate(AValue: string);
begin
  if FDocDate=AValue then Exit;
  FDocDate:=AValue;
  ModifiedProperty('DocDate');
end;

procedure TDocItem.SetDocErrors(AValue: string);
begin
  if FDocErrors=AValue then Exit;
  FDocErrors:=AValue;
  ModifiedProperty('DocErrors');
end;

procedure TDocItem.SetDocumentDate(AValue: TDateTime);
begin
  //
end;

procedure TDocItem.SetDocumentInvoiceDate(AValue: TDateTime);
begin

end;

procedure TDocItem.SetDocumentReceivedAt(AValue: TDateTime);
begin

end;

procedure TDocItem.SetDownloadDesc(AValue: string);
begin
  if FDownloadDesc=AValue then Exit;
  FDownloadDesc:=AValue;
  ModifiedProperty('DownloadDesc');
end;

procedure TDocItem.SetDownloadStatus(AValue: string);
begin
  if FDownloadStatus=AValue then Exit;
  FDownloadStatus:=AValue;
  ModifiedProperty('DownloadStatus');
end;

procedure TDocItem.SetErrors(AValue: TXSDStringArray);
begin
  if FErrors=AValue then Exit;
  FErrors:=AValue;
  ModifiedProperty('Errors');
end;

procedure TDocItem.SetInput(AValue: Boolean);
begin
  if FInput=AValue then Exit;
  FInput:=AValue;
  ModifiedProperty('Input');
end;

procedure TDocItem.SetInvoiceDate(AValue: string);
begin
  if FInvoiceDate=AValue then Exit;
  FInvoiceDate:=AValue;
  ModifiedProperty('InvoiceDate');
end;

procedure TDocItem.SetInvoiceNumber(AValue: string);
begin
  if FInvoiceNumber=AValue then Exit;
  FInvoiceNumber:=AValue;
  ModifiedProperty('InvoiceNumber');
end;

procedure TDocItem.SetNumber(AValue: string);
begin
  if FNumber=AValue then Exit;
  FNumber:=AValue;
  ModifiedProperty('Number');
end;

procedure TDocItem.SetPdfFile(AValue: string);
begin
  if FPdfFile=AValue then Exit;
  FPdfFile:=AValue;
  ModifiedProperty('PdfFile');
end;

procedure TDocItem.SetReceivedAt(AValue: string);
begin
  if FReceivedAt=AValue then Exit;
  FReceivedAt:=AValue;
  ModifiedProperty('ReceivedAt');
end;

procedure TDocItem.SetReceiverName(AValue: string);
begin
  if FreceiverName=AValue then Exit;
  FReceiverName:=AValue;
  ModifiedProperty('ReceiverName');
end;

procedure TDocItem.SetSenderName(AValue: string);
begin
  if FSenderName=AValue then Exit;
  FSenderName:=AValue;
  ModifiedProperty('SenderName');
end;

procedure TDocItem.SetStatus(AValue: string);
begin
  if FStatus=AValue then Exit;
  FStatus:=AValue;
  ModifiedProperty('Status');
end;

procedure TDocItem.SetTotal(AValue: Integer);
begin
  if FTotal=AValue then Exit;
  FTotal:=AValue;
  ModifiedProperty('Total');
end;

procedure TDocItem.SetVat(AValue: Integer);
begin
  if FVat=AValue then Exit;
  FVat:=AValue;
  ModifiedProperty('Vat');
end;

procedure TDocItem.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Number', 'number', [], '', -1, -1);
  RegisterProperty('DocDate', 'docDate', [], '', -1, -1);
  RegisterProperty('ReceivedAt', 'receivedAt', [], '', -1, -1);
  RegisterProperty('AType', 'type', [], '', -1, -1);
  RegisterProperty('Status', 'status', [], '', -1, -1);
  RegisterProperty('SenderName', 'senderName', [], '', -1, -1);
  RegisterProperty('ReceiverName', 'receiverName', [], '', -1, -1);
  RegisterProperty('InvoiceNumber', 'invoiceNumber', [], '', -1, -1);
  RegisterProperty('InvoiceDate', 'invoiceDate', [], '', -1, -1);
  RegisterProperty('Total', 'total', [], '', -1, -1);
  RegisterProperty('Vat', 'vat', [], '', -1, -1);
  RegisterProperty('Body', 'body', [], '', -1, -1);
  RegisterProperty('DownloadStatus', 'downloadStatus', [], '', -1, -1);
  RegisterProperty('DownloadDesc', 'downloadDesc', [], '', -1, -1);
  RegisterProperty('cisTotal', 'cisTotal', [], '', -1, -1);
  RegisterProperty('Content', 'content', [], '', -1, -1);
  RegisterProperty('Input', 'input', [], '', -1, -1);
  RegisterProperty('PdfFile', 'pdfFile', [], '', -1, -1);
  RegisterProperty('Errors', 'errors', [], '', -1, -1);
  RegisterProperty('DocErrors', 'docErrors', [], '', -1, -1);
  RegisterProperty('DocumentDataDto', 'documentDataDto', [], '', -1, -1);
end;

procedure TDocItem.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FDocumentDataDto:=TDocumentDataDto.Create;
end;

destructor TDocItem.Destroy;
begin
  FreeAndNil(FDocumentDataDto);
  inherited Destroy;
end;

{ TDocumentDataDto }

procedure TDocumentDataDto.SetEdoRecipient(AValue: string);
begin
  if FEdoRecipient=AValue then Exit;
  FEdoRecipient:=AValue;
  ModifiedProperty('EdoRecipient');
end;

procedure TDocumentDataDto.SetElr(AValue: Integer);
begin
  if FElr=AValue then Exit;
  FElr:=AValue;
  ModifiedProperty('Elr');
end;

procedure TDocumentDataDto.SetDid(AValue: string);
begin
  if FDid=AValue then Exit;
  FDid:=AValue;
  ModifiedProperty('Did');
end;

procedure TDocumentDataDto.SetErrors(AValue: string);
begin
  if FErrors=AValue then Exit;
  FErrors:=AValue;
  ModifiedProperty('Errors');
end;

procedure TDocumentDataDto.Setk_offset(AValue: Integer);
begin
  if Fk_offset=AValue then Exit;
  Fk_offset:=AValue;
  ModifiedProperty('k_offset');
end;

procedure TDocumentDataDto.Setk_partition(AValue: Integer);
begin
  if Fk_partition=AValue then Exit;
  Fk_partition:=AValue;
  ModifiedProperty('k_partition');
end;

procedure TDocumentDataDto.SetOst(AValue: string);
begin
  if FOst=AValue then Exit;
  FOst:=AValue;
  ModifiedProperty('Ost');
end;

procedure TDocumentDataDto.SetReceiptId(AValue: TXSDStringArray);
begin
  if FReceiptId=AValue then Exit;
  FReceiptId:=AValue;
  ModifiedProperty('ReceiptId');
end;

procedure TDocumentDataDto.SetSigningDate(AValue: Int64);
begin
  if FSigningDate=AValue then Exit;
  FSigningDate:=AValue;
  ModifiedProperty('SigningDate');
end;

procedure TDocumentDataDto.SetSignRecipientFio(AValue: string);
begin
  if FSignRecipientFio=AValue then Exit;
  FSignRecipientFio:=AValue;
  ModifiedProperty('SignRecipientFio');
end;

procedure TDocumentDataDto.SetT(AValue: Integer);
begin
  if FT=AValue then Exit;
  FT:=AValue;
  ModifiedProperty('T');
end;

procedure TDocumentDataDto.SetTrn(AValue: Integer);
begin
  if FTrn=AValue then Exit;
  FTrn:=AValue;
  ModifiedProperty('Trn');
end;

procedure TDocumentDataDto.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Ost', 'ost', [], '', -1, -1);
  RegisterProperty('Trn', 'trn', [], '', -1, -1);
  RegisterProperty('Errors', 'errors', [], '', -1, -1);
  RegisterProperty('SigningDate', 'signingDate', [], '', -1, -1);
  RegisterProperty('SignRecipientFio', 'signRecipientFio', [], '', -1, -1);
  RegisterProperty('EdoRecipient', 'edoRecipient', [], '', -1, -1);
  RegisterProperty('Did', 'did', [], '', -1, -1);
  RegisterProperty('T', 't', [], '', -1, -1);
  RegisterProperty('k_offset', 'k_offset', [], '', -1, -1);
  RegisterProperty('k_partition', 'k_partition', [], '', -1, -1);
  RegisterProperty('Elr', 'elr', [], '', -1, -1);
  RegisterProperty('ReceiptId', 'receiptId', [], '', -1, -1);
end;

procedure TDocumentDataDto.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

constructor TDocumentDataDto.Create;
begin
  inherited Create;
  FIgnoreReadUndefProps:=true;
end;

destructor TDocumentDataDto.Destroy;
begin
  inherited Destroy;
end;

end.

