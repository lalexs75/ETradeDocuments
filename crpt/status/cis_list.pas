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

unit cis_list;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, JSONObjects, AbstractSerializationObjects;

type

  { TCISItem }

  TCISItem = class(TJSONSerializationObject)
  private
    FAgentName: string;
    FBrand: string;
    FChildren: TXSDStringArray;
    FCIS: string;
    FCountChildren: Integer;
    FEmissionDate: Int64;
    FGTIN: string;
    FIntroducedDate: Int64;
    FLastDocId: string;
    FLastStatusChangeDate: string;
    FNextCises: string;
    FOwnerInn: string;
    FOwnerName: string;
    FPackageType: string;
    FParent: string;
    FPrevCises: string;
    FProducedDate: Int64;
    FProducerName: string;
    FProductGroup: string;
    FProductName: string;
    FStatus: string;
    FStatusEx: string;
    FTNVED10: string;
    procedure SetAgentName(AValue: string);
    procedure SetBrand(AValue: string);
    procedure SetChildren(AValue: TXSDStringArray);
    procedure SetCIS(AValue: string);
    procedure SetCountChildren(AValue: Integer);
    procedure SetEmissionDate(AValue: Int64);
    procedure SetGTIN(AValue: string);
    procedure SetIntroducedDate(AValue: Int64);
    procedure SetLastDocId(AValue: string);
    procedure SetLastStatusChangeDate(AValue: string);
    procedure SetNextCises(AValue: string);
    procedure SetOwnerInn(AValue: string);
    procedure SetOwnerName(AValue: string);
    procedure SetPackageType(AValue: string);
    procedure SetParent(AValue: string);
    procedure SetPrevCises(AValue: string);
    procedure SetProducedDate(AValue: Int64);
    procedure SetProducerName(AValue: string);
    procedure SetProductGroup(AValue: string);
    procedure SetProductName(AValue: string);
    procedure SetStatus(AValue: string);
    procedure SetStatusEx(AValue: string);
    procedure SetTNVED10(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property CIS:string read FCIS write SetCIS;
    property GTIN:string read FGTIN write SetGTIN;
    property ProducerName:string read FProducerName write SetProducerName;
    property Status:string read FStatus write SetStatus;
    property StatusEx:string read FStatusEx write SetStatusEx;
    property EmissionDate:Int64 read FEmissionDate write SetEmissionDate;
    property PackageType:string read FPackageType write SetPackageType;
    property OwnerName:string read FOwnerName write SetOwnerName;
    property OwnerInn:string read FOwnerInn write SetOwnerInn;
    property ProductName:string read FProductName write SetProductName;
    property Brand:string read FBrand write SetBrand;
    property PrevCises:string read FPrevCises write SetPrevCises;
    property NextCises:string read FNextCises write SetNextCises;
    property CountChildren:Integer read FCountChildren write SetCountChildren;
    property LastDocId:string read FLastDocId write SetLastDocId;
    property IntroducedDate:Int64 read FIntroducedDate write SetIntroducedDate;
    property AgentName:string read FAgentName write SetAgentName;
    property LastStatusChangeDate:string read FLastStatusChangeDate write SetLastStatusChangeDate;
    property ProductGroup:string read FProductGroup write SetProductGroup;
    property ProducedDate:Int64 read FProducedDate write SetProducedDate;
    property Children:TXSDStringArray read FChildren write SetChildren;
    property TNVED10:string read FTNVED10 write SetTNVED10;
    property Parent:string read FParent write SetParent;
  end;
  TCISItemList = specialize GXMLSerializationObjectList<TCISItem>;


  { TCISItems }

  TCISItems = class(TJSONSerializationObject)
  private
    FItems: TCISItemList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create; override;
    destructor Destroy; override;
  published
    property Items:TCISItemList read FItems;
  end;
implementation

{ TCISItems }

procedure TCISItems.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Items', 'Items', [xsaDefault], '', -1, -1);
end;

procedure TCISItems.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FItems:=TCISItemList.Create;
end;

constructor TCISItems.Create;
begin
  inherited Create;
  FIgnoreReadUndefProps:=true;
end;

destructor TCISItems.Destroy;
begin
  FreeAndNil(FItems);
  inherited Destroy;
end;

{ TCISItem }

procedure TCISItem.SetAgentName(AValue: string);
begin
  if FAgentName=AValue then Exit;
  FAgentName:=AValue;
  ModifiedProperty('AgentName');
end;

procedure TCISItem.SetBrand(AValue: string);
begin
  if FBrand=AValue then Exit;
  FBrand:=AValue;
  ModifiedProperty('Brand');
end;

procedure TCISItem.SetChildren(AValue: TXSDStringArray);
begin
  if FChildren=AValue then Exit;
  FChildren:=AValue;
  ModifiedProperty('Children');
end;

procedure TCISItem.SetCIS(AValue: string);
begin
  if FCIS=AValue then Exit;
  FCIS:=AValue;
  ModifiedProperty('CIS');
end;

procedure TCISItem.SetCountChildren(AValue: Integer);
begin
  if FCountChildren=AValue then Exit;
  FCountChildren:=AValue;
  ModifiedProperty('CountChildren');
end;

procedure TCISItem.SetEmissionDate(AValue: Int64);
begin
  if FEmissionDate=AValue then Exit;
  FEmissionDate:=AValue;
  ModifiedProperty('EmissionDate');
end;

procedure TCISItem.SetGTIN(AValue: string);
begin
  if FGTIN=AValue then Exit;
  FGTIN:=AValue;
  ModifiedProperty('GTIN');
end;

procedure TCISItem.SetIntroducedDate(AValue: Int64);
begin
  if FIntroducedDate=AValue then Exit;
  FIntroducedDate:=AValue;
  ModifiedProperty('IntroducedDate');
end;

procedure TCISItem.SetLastDocId(AValue: string);
begin
  if FLastDocId=AValue then Exit;
  FLastDocId:=AValue;
  ModifiedProperty('LastDocId');
end;

procedure TCISItem.SetLastStatusChangeDate(AValue: string);
begin
  if FLastStatusChangeDate=AValue then Exit;
  FLastStatusChangeDate:=AValue;
  ModifiedProperty('LastStatusChangeDate');
end;

procedure TCISItem.SetNextCises(AValue: string);
begin
  if FNextCises=AValue then Exit;
  FNextCises:=AValue;
  ModifiedProperty('NextCises');
end;

procedure TCISItem.SetOwnerInn(AValue: string);
begin
  if FOwnerInn=AValue then Exit;
  FOwnerInn:=AValue;
  ModifiedProperty('OwnerInn');
end;

procedure TCISItem.SetOwnerName(AValue: string);
begin
  if FOwnerName=AValue then Exit;
  FOwnerName:=AValue;
  ModifiedProperty('OwnerName');
end;

procedure TCISItem.SetPackageType(AValue: string);
begin
  if FPackageType=AValue then Exit;
  FPackageType:=AValue;
  ModifiedProperty('PackageType');
end;

procedure TCISItem.SetParent(AValue: string);
begin
  if FParent=AValue then Exit;
  FParent:=AValue;
  ModifiedProperty('Parent');
end;

procedure TCISItem.SetPrevCises(AValue: string);
begin
  if FPrevCises=AValue then Exit;
  FPrevCises:=AValue;
  ModifiedProperty('PrevCises');
end;

procedure TCISItem.SetProducedDate(AValue: Int64);
begin
  if FProducedDate=AValue then Exit;
  FProducedDate:=AValue;
  ModifiedProperty('ProducedDate');
end;

procedure TCISItem.SetProducerName(AValue: string);
begin
  if FProducerName=AValue then Exit;
  FProducerName:=AValue;
  ModifiedProperty('ProducerName');
end;

procedure TCISItem.SetProductGroup(AValue: string);
begin
  if FProductGroup=AValue then Exit;
  FProductGroup:=AValue;
  ModifiedProperty('ProductGroup');
end;

procedure TCISItem.SetProductName(AValue: string);
begin
  if FProductName=AValue then Exit;
  FProductName:=AValue;
  ModifiedProperty('ProductName');
end;

procedure TCISItem.SetStatus(AValue: string);
begin
  if FStatus=AValue then Exit;
  FStatus:=AValue;
  ModifiedProperty('Status');
end;

procedure TCISItem.SetStatusEx(AValue: string);
begin
  if FStatusEx=AValue then Exit;
  FStatusEx:=AValue;
  ModifiedProperty('StatusEx');
end;

procedure TCISItem.SetTNVED10(AValue: string);
begin
  if FTNVED10=AValue then Exit;
  FTNVED10:=AValue;
  ModifiedProperty('TNVED10');
end;

procedure TCISItem.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('CIS', 'cis', [], '', -1, -1);
  RegisterProperty('GTIN', 'gtin', [], '', -1, -1);
  RegisterProperty('ProducerName', 'producerName', [], '', -1, -1);
  RegisterProperty('Status', 'status', [], '', -1, -1);
  RegisterProperty('StatusEx', 'statusEx', [], '', -1, -1);
  RegisterProperty('EmissionDate', 'emissionDate', [], '', -1, -1);
  RegisterProperty('PackageType', 'packageType', [], '', -1, -1);
  RegisterProperty('OwnerName', 'ownerName', [], '', -1, -1);
  RegisterProperty('OwnerInn', 'ownerInn', [], '', -1, -1);
  RegisterProperty('ProductName', 'productName', [], '', -1, -1);
  RegisterProperty('Brand', 'brand', [], '', -1, -1);
  RegisterProperty('PrevCises', 'prevCises', [], '', -1, -1);
  RegisterProperty('NextCises', 'nextCises', [], '', -1, -1);
  RegisterProperty('CountChildren', 'countChildren', [], '', -1, -1);
  RegisterProperty('LastDocId', 'lastDocId', [], '', -1, -1);
  RegisterProperty('IntroducedDate', 'introducedDate', [], '', -1, -1);
  RegisterProperty('AgentName', 'agentName', [], '', -1, -1);
  RegisterProperty('LastStatusChangeDate', 'lastStatusChangeDate', [], '', -1, -1);
  RegisterProperty('ProductGroup', 'productGroup', [], '', -1, -1);
  RegisterProperty('ProducedDate', 'producedDate', [], '', -1, -1);
  RegisterProperty('Children', 'children', [], '', -1, -1);
  RegisterProperty('TNVED10', 'tnVed10', [], '', -1, -1);
  RegisterProperty('Parent', 'parent', [], '', -1, -1);
end;

procedure TCISItem.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

constructor TCISItem.Create;
begin
  inherited Create;
  FIgnoreReadUndefProps:=true;
end;

destructor TCISItem.Destroy;
begin
  inherited Destroy;
end;
end.


