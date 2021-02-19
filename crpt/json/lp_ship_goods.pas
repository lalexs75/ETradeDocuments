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

unit lp_ship_goods;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects, AbstractSerializationObjects;

type

  { TChildren }

  TChildren = class(TJSONSerializationObject)
  private
    FProductDescription: string;
    FUITCode: string;
    procedure SetProductDescription(AValue: string);
    procedure SetUITCode(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  public
  published
    property UITCode:string read FUITCode write SetUITCode;
    property ProductDescription:string read FProductDescription write SetProductDescription;
  end;
  TChildrens = specialize GXMLSerializationObjectList<TChildren>;

  { TProduct }

  TProduct = class(TJSONSerializationObject)
  private
    FChildren: TChildrens;
    FCountChildren: Integer;
    FProductCost: Double;
    FProductDescription: string;
    FProductTax: Double;
    FUITCode: string;
    FUITUCode: string;
    procedure SetCountChildren(AValue: Integer);
    procedure SetProductCost(AValue: Double);
    procedure SetProductDescription(AValue: string);
    procedure SetProductTax(AValue: Double);
    procedure SetUITCode(AValue: string);
    procedure SetUITUCode(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  public
  published
    property UITCode:string read FUITCode write SetUITCode;
    property UITUCode:string read FUITUCode write SetUITUCode;
    property ProductDescription:string read FProductDescription write SetProductDescription;
    property ProductCost:Double read FProductCost write SetProductCost;
    property ProductTax:Double read FProductTax write SetProductTax;
    property CountChildren:Integer read FCountChildren write SetCountChildren;
    property Children:TChildrens read FChildren;
  end;
  TProducts = specialize GXMLSerializationObjectList<TProduct>;
  { TShipGoods }

  TShipGoods = class(TJSONSerializationObject)
  private
    FDocumentDate: string;
    FDocumentNum: string;
    FProducts: TProducts;
    FReceiver: string;
    FReceiverInn: string;
    FRequestType: string;
    FSale: Boolean;
    FSender: string;
    FSenderInn: string;
    FStContractId: string;
    FToNotParticipant: Boolean;
    FTransferDate: string;
    FTurnoverType: string;
    FWithdrawalFromTurnover: Boolean;
    procedure SetDocumentDate(AValue: string);
    procedure SetDocumentNum(AValue: string);
    procedure SetReceiver(AValue: string);
    procedure SetReceiverInn(AValue: string);
    procedure SetRequestType(AValue: string);
    procedure SetSale(AValue: Boolean);
    procedure SetSender(AValue: string);
    procedure SetSenderInn(AValue: string);
    procedure SetStContractId(AValue: string);
    procedure SetToNotParticipant(AValue: Boolean);
    procedure SetTransferDate(AValue: string);
    procedure SetTurnoverType(AValue: string);
    procedure SetWithdrawalFromTurnover(AValue: Boolean);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  public
  published
    property Sale:Boolean read FSale write SetSale;
    property TurnoverType:string read FTurnoverType write SetTurnoverType;
    property RequestType:string read FRequestType write SetRequestType;
    property SenderInn:string read FSenderInn write SetSenderInn;
    property DocumentNum:string read FDocumentNum write SetDocumentNum;
    property DocumentDate:string read FDocumentDate write SetDocumentDate;
    property WithdrawalFromTurnover:Boolean read FWithdrawalFromTurnover write SetWithdrawalFromTurnover;
    property TransferDate:string read FTransferDate write SetTransferDate;
    property ReceiverInn:string read FReceiverInn write SetReceiverInn;
    property ToNotParticipant:Boolean read FToNotParticipant write SetToNotParticipant;
    property Sender:string read FSender write SetSender;
    property Receiver:string read FReceiver write SetReceiver;
    property StContractId:string read FStContractId write SetStContractId;
    property Products:TProducts read FProducts;
  end;

implementation

{ TChildren }

procedure TChildren.SetProductDescription(AValue: string);
begin
  if FProductDescription=AValue then Exit;
  FProductDescription:=AValue;
  ModifiedProperty('ProductDescription');
end;

procedure TChildren.SetUITCode(AValue: string);
begin
  if FUITCode=AValue then Exit;
  FUITCode:=AValue;
  ModifiedProperty('UITCode');
end;

procedure TChildren.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('UITCode', 'uit_code', [], '', -1, -1);
  RegisterProperty('ProductDescription', 'product_description', [], '', -1, -1);
end;

procedure TChildren.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TChildren.Destroy;
begin
  inherited Destroy;
end;

{ TProduct }

procedure TProduct.SetCountChildren(AValue: Integer);
begin
  if FCountChildren=AValue then Exit;
  FCountChildren:=AValue;
  ModifiedProperty('CountChildren');
end;

procedure TProduct.SetProductCost(AValue: Double);
begin
  if FProductCost=AValue then Exit;
  FProductCost:=AValue;
  ModifiedProperty('ProductCost');
end;

procedure TProduct.SetProductDescription(AValue: string);
begin
  if FProductDescription=AValue then Exit;
  FProductDescription:=AValue;
  ModifiedProperty('ProductDescription');
end;

procedure TProduct.SetProductTax(AValue: Double);
begin
  if FProductTax=AValue then Exit;
  FProductTax:=AValue;
  ModifiedProperty('ProductTax');
end;

procedure TProduct.SetUITCode(AValue: string);
begin
  if FUITCode=AValue then Exit;
  FUITCode:=AValue;
  ModifiedProperty('UITCode');
end;

procedure TProduct.SetUITUCode(AValue: string);
begin
  if FUITUCode=AValue then Exit;
  FUITUCode:=AValue;
  ModifiedProperty('UITUCode');
end;

procedure TProduct.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('UITCode', 'uit_code', [], '', -1, -1);
  RegisterProperty('UITUCode', 'uitu_code', [], '', -1, -1);
  RegisterProperty('ProductDescription', 'product_description', [], '', -1, -1);
  RegisterProperty('ProductCost', 'product_cost', [], '', -1, -1);
  RegisterProperty('ProductTax', 'product_tax', [], '', -1, -1);
  RegisterProperty('CountChildren', 'count_children', [], '', -1, -1);
  RegisterProperty('Children', 'children', [], '', -1, -1);
end;

procedure TProduct.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FChildren:=TChildrens.Create;
end;

destructor TProduct.Destroy;
begin
  FreeAndNil(FChildren);
  inherited Destroy;
end;

{ TShipGoods }

procedure TShipGoods.SetDocumentDate(AValue: string);
begin
  if FDocumentDate=AValue then Exit;
  FDocumentDate:=AValue;
  ModifiedProperty('DocumentDate');
end;

procedure TShipGoods.SetDocumentNum(AValue: string);
begin
  if FDocumentNum=AValue then Exit;
  FDocumentNum:=AValue;
  ModifiedProperty('DocumentNum');
end;

procedure TShipGoods.SetReceiver(AValue: string);
begin
  if FReceiver=AValue then Exit;
  FReceiver:=AValue;
  ModifiedProperty('Receiver');
end;

procedure TShipGoods.SetReceiverInn(AValue: string);
begin
  if FReceiverInn=AValue then Exit;
  FReceiverInn:=AValue;
  ModifiedProperty('ReceiverInn');
end;

procedure TShipGoods.SetRequestType(AValue: string);
begin
  if FRequestType=AValue then Exit;
  FRequestType:=AValue;
  ModifiedProperty('RequestType');
end;

procedure TShipGoods.SetSale(AValue: Boolean);
begin
  if FSale=AValue then Exit;
  FSale:=AValue;
  ModifiedProperty('Sale');
end;

procedure TShipGoods.SetSender(AValue: string);
begin
  if FSender=AValue then Exit;
  FSender:=AValue;
  ModifiedProperty('Sender');
end;

procedure TShipGoods.SetSenderInn(AValue: string);
begin
  if FSenderInn=AValue then Exit;
  FSenderInn:=AValue;
  ModifiedProperty('SenderInn');
end;

procedure TShipGoods.SetStContractId(AValue: string);
begin
  if FStContractId=AValue then Exit;
  FStContractId:=AValue;
  ModifiedProperty('StContractId');
end;

procedure TShipGoods.SetToNotParticipant(AValue: Boolean);
begin
  if FToNotParticipant=AValue then Exit;
  FToNotParticipant:=AValue;
  ModifiedProperty('ToNotParticipant');
end;

procedure TShipGoods.SetTransferDate(AValue: string);
begin
  if FTransferDate=AValue then Exit;
  FTransferDate:=AValue;
  ModifiedProperty('TransferDate');
end;

procedure TShipGoods.SetTurnoverType(AValue: string);
begin
  if FTurnoverType=AValue then Exit;
  FTurnoverType:=AValue;
  ModifiedProperty('TurnoverType');
end;

procedure TShipGoods.SetWithdrawalFromTurnover(AValue: Boolean);
begin
  if FWithdrawalFromTurnover=AValue then Exit;
  FWithdrawalFromTurnover:=AValue;
  ModifiedProperty('WithdrawalFromTurnover');
end;

procedure TShipGoods.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Sale', 'sale', [], '', -1, -1);
  RegisterProperty('TurnoverType', 'turnover_type', [], '', -1, -1);
  RegisterProperty('RequestType', 'request_type', [], '', -1, -1);
  RegisterProperty('SenderInn', 'sender_inn', [], '', -1, -1);
  RegisterProperty('DocumentNum', 'document_num', [], '', -1, -1);
  RegisterProperty('DocumentDate', 'document_date', [], '', -1, -1);
  RegisterProperty('WithdrawalFromTurnover', 'withdrawal_from_turnover', [], '', -1, -1);
  RegisterProperty('TransferDate', 'transfer_date', [], '', -1, -1);
  RegisterProperty('ReceiverInn', 'receiver_inn', [], '', -1, -1);
  RegisterProperty('ToNotParticipant', 'to_not_participant', [], '', -1, -1);
  RegisterProperty('Sender', 'sender', [], '', -1, -1);
  RegisterProperty('Receiver', 'receiver', [], '', -1, -1);
  RegisterProperty('StContractId', 'st_contract_id', [], '', -1, -1);

  RegisterProperty('Products', 'products', [], '', -1, -1);
end;

procedure TShipGoods.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FProducts:=TProducts.Create;
end;

destructor TShipGoods.Destroy;
begin
  FreeAndNil(FProducts);
  inherited Destroy;
end;

end.

