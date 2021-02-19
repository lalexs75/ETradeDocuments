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

unit crpt_vvod_import_fts;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, AbstractSerializationObjects;

type
  TProduct_Date = TDate;

  { TProduct }

  TProduct = class(TXmlSerializationObject)
  private
    Fki: string;
    procedure Setki(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property ki:string read Fki write Setki;
  end;
  TProducts = specialize GXMLSerializationObjectList<TProduct>;

  { TProductList }

  TProductList = class(TXmlSerializationObject)
  private
    FProducts: TProducts;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property Products:TProducts read FProducts;
  end;


  { TVvod_import_fts }

  TVvod_import_fts = class(TXmlSerializationObject)
  private
    Fdeclaration_date: TProduct_Date;
    Fdeclaration_number: string;
    FProductList: TProductList;
    Ftrade_participant_inn: string;
    FVersion: Integer;
    procedure Setdeclaration_date(AValue: TProduct_Date);
    procedure Setdeclaration_number(AValue: string);
    procedure Settrade_participant_inn(AValue: string);
    procedure SetVersion(AValue: Integer);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
    function RootNodeName:string; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property Version:Integer read FVersion write SetVersion;
    property trade_participant_inn:string read Ftrade_participant_inn write Settrade_participant_inn;
    property declaration_number:string read Fdeclaration_number write Setdeclaration_number;
    property declaration_date:TProduct_Date read Fdeclaration_date write Setdeclaration_date;
    property ProductList:TProductList read FProductList;
  end;

implementation

{ TProductList }

procedure TProductList.InternalRegisterPropertys;
begin
  inherited InternalRegisterPropertys;
  RegisterProperty('Products', 'product', [], '', -1, -1);
end;

procedure TProductList.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FProducts:=TProducts.Create;
end;

destructor TProductList.Destroy;
begin
  FreeAndNil(FProducts);
  inherited Destroy;
end;

{ TProduct }

procedure TProduct.Setki(AValue: string);
begin
  if Fki=AValue then Exit;
  Fki:=AValue;
  ModifiedProperty('ki');
end;

procedure TProduct.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', -1, -1);
end;

procedure TProduct.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TProduct.Destroy;
begin
  inherited Destroy;
end;

{ TVvod_import_fts }

procedure TVvod_import_fts.Setdeclaration_date(AValue: TProduct_Date);
begin
  if Fdeclaration_date=AValue then Exit;
  Fdeclaration_date:=AValue;
  ModifiedProperty('declaration_date');
end;

procedure TVvod_import_fts.Setdeclaration_number(AValue: string);
begin
  if Fdeclaration_number=AValue then Exit;
  Fdeclaration_number:=AValue;
  ModifiedProperty('declaration_number');
end;

procedure TVvod_import_fts.Settrade_participant_inn(AValue: string);
begin
  if Ftrade_participant_inn=AValue then Exit;
  Ftrade_participant_inn:=AValue;
  ModifiedProperty('trade_participant_inn');
end;

procedure TVvod_import_fts.SetVersion(AValue: Integer);
begin
  if FVersion=AValue then Exit;
  FVersion:=AValue;
  ModifiedProperty('Version');
end;

procedure TVvod_import_fts.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('trade_participant_inn', 'trade_participant_inn', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('declaration_number', 'declaration_number', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('declaration_date', 'declaration_date', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('Version', 'version', [], '', -1, -1);
    P.DefaultValue:='1';
  P:=RegisterProperty('ProductList', 'products_list', [], '', -1, -1);
end;

procedure TVvod_import_fts.InternalInitChilds;
begin
  inherited InternalInitChilds;
  FProductList:=TProductList.Create;
end;

function TVvod_import_fts.RootNodeName: string;
begin
  Result:='vvod_import_fts';
end;

constructor TVvod_import_fts.Create;
begin
  inherited Create;
  Version:=1;
end;

destructor TVvod_import_fts.Destroy;
begin
  FreeAndNil(FProductList);
  inherited Destroy;
end;

end.

