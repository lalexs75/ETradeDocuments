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

unit lp_ship_goods_csv;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils;

type

  { TProduct }

  TProduct = class
  private
    FProductCost: Currency;
    FProductTax: Currency;
    FUITCode: string;
    FUITUCode: string;
    procedure SetProductCost(AValue: Currency);
    procedure SetProductTax(AValue: Currency);
    procedure SetUITCode(AValue: string);
    procedure SetUITUCode(AValue: string);
  public
    //КИ,
    property UITCode:string read FUITCode write SetUITCode;
    //КИТУ,
    property UITUCode:string read FUITUCode write SetUITUCode;
    //Цена за единицу,
    property ProductCost:Currency read FProductCost write SetProductCost;
    //Сумма НДС
    property ProductTax:Currency read FProductTax write SetProductTax;
  end;
  TProductClass = class of TProduct;

  { TProducts }
  TProductsEnumerator = class;
  TProducts = class
  private
    FList:TFPList;
    FBaseClass:TProductClass;
    function GetCount: Integer;
    function GetItem(AIndex: Integer): TProduct;
  protected
    function InternalAddObject:TProduct;
    function InternalGetItem(AIndex: Integer):TProduct;
  public
    constructor Create(ABaseClass:TProductClass);
    procedure Clear;
    destructor Destroy; override;
    function GetEnumerator: TProductsEnumerator;

    property Count:Integer read GetCount;
    property Items[AIndex:Integer]:TProduct read GetItem; default;
  end;

  { TProductsEnumerator }

  TProductsEnumerator = class
  private
    FList: TProducts;
    FPosition: Integer;
  public
    constructor Create(AList: TProducts);
    function GetCurrent: TProduct;
    function MoveNext: Boolean;
    property Current: TProduct read GetCurrent;
  end;

  { TShipGoods }

  TShipGoods = class(TObject)
  private
    FDocumentDate: string;
    FDocumentNum: string;
    FOwnerInn: string;
    FProducts: TProducts;
    FReceiverInn: string;
    FSale: string;
    FSenderInn: string;
    FStContractId: string;
    FTransferDate: string;
    FTurnoverType: string;
    FVersion: string;
    FwithdrawalDate: string;
    FWithdrawalType: string;
    procedure SetDocumentDate(AValue: string);
    procedure SetDocumentNum(AValue: string);
    procedure SetOwnerInn(AValue: string);
    procedure SetReceiverInn(AValue: string);
    procedure SetSale(AValue: string);
    procedure SetSenderInn(AValue: string);
    procedure SetStContractId(AValue: string);
    procedure SetTransferDate(AValue: string);
    procedure SetTurnoverType(AValue: string);
    procedure SetVersion(AValue: string);
    procedure SetwithdrawalDate(AValue: string);
    procedure SetWithdrawalType(AValue: string);
  private
    procedure DoLoadHeader(S:string);
    procedure DoLoadRow(S:string);
  public
    constructor Create;
    destructor Destroy; override;
    procedure Clear;
    procedure LoadFromFile(AFileName:string);
    procedure LoadFromStream(AStream:TStream); virtual;
  published
    //ИНН отправителя,
    property SenderInn:string read FSenderInn write SetSenderInn;
    //ИНН получателя,
    property ReceiverInn:string read FReceiverInn write SetReceiverInn;
    //ИНН собственника,
    property OwnerInn:string read FOwnerInn write SetOwnerInn;
    //Дата отгрузки товара,
    property TransferDate:string read FTransferDate write SetTransferDate;
    //Номер первичного документа,
    property DocumentNum:string read FDocumentNum write SetDocumentNum;
    //Дата первичного документа,
    property DocumentDate:string read FDocumentDate write SetDocumentDate;
    //Вид оборота товаров,
    property TurnoverType:string read FTurnoverType write SetTurnoverType;
    //Причина вывода из оборота,
    property WithdrawalType:string read FWithdrawalType write SetWithdrawalType;
    //Дата вывода из оборота,
    property withdrawalDate:string read FwithdrawalDate write SetwithdrawalDate;
    //Идентификатор гос.контракта,
    property StContractId:string read FStContractId write SetStContractId;
    //Отгрузка неучастнику,
    property Sale:string read FSale write SetSale;
    //Версия
    property Version:string read FVersion write SetVersion;

    property Products:TProducts read FProducts;
  end;

implementation
uses StrUtils;

{ TProductsEnumerator }

constructor TProductsEnumerator.Create(AList: TProducts);
begin
  FList := AList;
  FPosition := -1;
end;

function TProductsEnumerator.GetCurrent: TProduct;
begin
  Result := TProduct(FList.FList[FPosition]);
end;

function TProductsEnumerator.MoveNext: Boolean;
begin
  Inc(FPosition);
  Result := FPosition < FList.Count;
end;

{ TProduct }

procedure TProduct.SetProductCost(AValue: Currency);
begin
  if FProductCost=AValue then Exit;
  FProductCost:=AValue;
end;

procedure TProduct.SetProductTax(AValue: Currency);
begin
  if FProductTax=AValue then Exit;
  FProductTax:=AValue;
end;

procedure TProduct.SetUITCode(AValue: string);
begin
  if FUITCode=AValue then Exit;
  FUITCode:=AValue;
end;

procedure TProduct.SetUITUCode(AValue: string);
begin
  if FUITUCode=AValue then Exit;
  FUITUCode:=AValue;
end;

{ TProducts }

function TProducts.GetCount: Integer;
begin
  Result:=FList.Count;
end;

function TProducts.GetItem(AIndex: Integer): TProduct;
begin
  Result:=TProduct(InternalGetItem(AIndex));
end;

function TProducts.InternalAddObject: TProduct;
begin
  Result:=FBaseClass.Create;
  FList.Add(Result);
end;

function TProducts.InternalGetItem(AIndex: Integer): TProduct;
begin
  Result:=TProduct(FList[AIndex]);
end;

constructor TProducts.Create(ABaseClass: TProductClass);
begin
  inherited Create;
  FList:=TFPList.Create;
  FBaseClass:=ABaseClass;
end;

procedure TProducts.Clear;
var
  i: Integer;
begin
  for i:=0 to FList.Count-1 do
    TProduct(FList[i]).Free;
  FList.Clear;
end;

destructor TProducts.Destroy;
begin
  Clear;
  FreeAndNil(FList);
  inherited Destroy;
end;

function TProducts.GetEnumerator: TProductsEnumerator;
begin
  Result:=TProductsEnumerator.Create(Self);
end;

{ TShipGoods }

procedure TShipGoods.DoLoadHeader(S: string);
var
  A: TStringArray;
begin
   //ИНН отправителя,ИНН получателя,ИНН собственника,Дата отгрузки товара,Номер первичного документа,Дата первичного документа,Вид оборота товаров,Причина вывода из оборота, Дата вывода из оборота,Идентификатор гос.контракта,Отгрузка неучастнику,Версия
  A:=S.Split(',', '"');
  SenderInn:=A[0]; //ИНН отправителя,
  ReceiverInn:=A[1]; //ИНН получателя,
  OwnerInn:=A[2]; //ИНН собственника,
  TransferDate:=A[3]; //Дата отгрузки товара,
  DocumentNum:=A[4]; //Номер первичного документа,
  DocumentDate:=A[5]; //Дата первичного документа,
  TurnoverType:=A[6]; //Вид оборота товаров,
  WithdrawalType:=A[7]; //Причина вывода из оборота,
  withdrawalDate:=A[8]; //Дата вывода из оборота,
  StContractId:=A[9]; //Идентификатор гос.контракта,
  Sale:=A[10]; //Отгрузка неучастнику,
  Version:=A[11]; //Версия
end;

procedure TShipGoods.DoLoadRow(S: string);
var
  R: TProduct;
  A: TStringArray;
begin
  //КИ,КИТУ,Цена за единицу,Сумма НДС
  A:=S.Split(',', '"');
  R:=FProducts.InternalAddObject;
  R.FUITCode:=AnsiDequotedStr(A[0], '"');
  R.FUITUCode:=AnsiDequotedStr(A[1], '"');
  Val(A[2], R.FProductCost);
  Val(A[3], R.FProductTax);
end;

procedure TShipGoods.SetDocumentDate(AValue: string);
begin
  if FDocumentDate=AValue then Exit;
  FDocumentDate:=AValue;
end;

procedure TShipGoods.SetDocumentNum(AValue: string);
begin
  if FDocumentNum=AValue then Exit;
  FDocumentNum:=AValue;
end;

procedure TShipGoods.SetOwnerInn(AValue: string);
begin
  if FOwnerInn=AValue then Exit;
  FOwnerInn:=AValue;
end;

procedure TShipGoods.SetReceiverInn(AValue: string);
begin
  if FReceiverInn=AValue then Exit;
  FReceiverInn:=AValue;
end;

procedure TShipGoods.SetSale(AValue: string);
begin
  if FSale=AValue then Exit;
  FSale:=AValue;
end;

procedure TShipGoods.SetSenderInn(AValue: string);
begin
  if FSenderInn=AValue then Exit;
  FSenderInn:=AValue;
end;

procedure TShipGoods.SetStContractId(AValue: string);
begin
  if FStContractId=AValue then Exit;
  FStContractId:=AValue;
end;

procedure TShipGoods.SetTransferDate(AValue: string);
begin
  if FTransferDate=AValue then Exit;
  FTransferDate:=AValue;
end;

procedure TShipGoods.SetTurnoverType(AValue: string);
begin
  if FTurnoverType=AValue then Exit;
  FTurnoverType:=AValue;
end;

procedure TShipGoods.SetVersion(AValue: string);
begin
  if FVersion=AValue then Exit;
  FVersion:=AValue;
end;

procedure TShipGoods.SetwithdrawalDate(AValue: string);
begin
  if FwithdrawalDate=AValue then Exit;
  FwithdrawalDate:=AValue;
end;

procedure TShipGoods.SetWithdrawalType(AValue: string);
begin
  if FWithdrawalType=AValue then Exit;
  FWithdrawalType:=AValue;
end;

constructor TShipGoods.Create;
begin
  inherited Create;
  FProducts:=TProducts.Create(TProduct);
end;

destructor TShipGoods.Destroy;
begin
  FreeAndNil(FProducts);
  inherited Destroy;
end;

procedure TShipGoods.Clear;
begin
  FProducts.Clear;
end;

procedure TShipGoods.LoadFromFile(AFileName: string);
var
  F: TFileStream;
begin
  F:=TFileStream.Create(AFileName, fmOpenRead);
  LoadFromStream(F);
  F.Free;
end;

procedure TShipGoods.LoadFromStream(AStream: TStream);
var
  St:TStringList;
  i: Integer;
begin
  Clear;
  if not Assigned(AStream) then Exit;
  St:=TStringList.Create;
  St.LoadFromStream(AStream);

  DoLoadHeader(ST[1]);

  for i:=4 to ST.Count-1 do
    DoLoadRow(ST[i]);
  St.Free;
end;

end.

