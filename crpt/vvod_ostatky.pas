{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020 Lagunov Aleksey alexs75@yandex.ru

  base on docs from http://api-docs.diadoc.ru

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

unit vvod_ostatky;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, LP_base_types_v3;

type

  {  Forward declarations  }
  Tvvod_ostatky = class;
  Tvvod_ostatky_element = class;
  Tvvod_ostatky_products_list = class;
  Tvvod_ostatky_products_list_product = class;

  {  Generic classes for collections  }
  Tvvod_ostatkyList = specialize GXMLSerializationObjectList<Tvvod_ostatky>;
  Tvvod_ostatky_elementList = specialize GXMLSerializationObjectList<Tvvod_ostatky_element>;
  Tvvod_ostatky_products_listList = specialize GXMLSerializationObjectList<Tvvod_ostatky_products_list>;
  Tvvod_ostatky_products_list_productList = specialize GXMLSerializationObjectList<Tvvod_ostatky_products_list_product>;

  {  Tvvod_ostatky  }
  //Ввод в оборот. Остатки
  Tvvod_ostatky = class(TXmlSerializationObject)
  private
    Ftrade_participant_inn:Tinn_type;
    Fproducts_list:Tvvod_ostatky_products_list;
    Faction_id:Longint;
    Fversion:Longint;
    procedure Settrade_participant_inn( AValue:Tinn_type);
    procedure Setaction_id( AValue:Longint);
    procedure Setversion( AValue:Longint);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //ИНН участника оборота
    property trade_participant_inn:Tinn_type read Ftrade_participant_inn write Settrade_participant_inn;
    //Параметры товаров
    property products_list:Tvvod_ostatky_products_list read Fproducts_list;
    property action_id:Longint read Faction_id write Setaction_id;
    property version:Longint read Fversion write Setversion;
  end;

  {  Tvvod_ostatky_element  }
  Tvvod_ostatky_element = class(Tvvod_ostatky)
  private
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
    function RootNodeName:string; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
  end;

  {  Tvvod_ostatky_products_list  }
  Tvvod_ostatky_products_list = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_ostatky_products_list_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Параметры товара
    property product:Tvvod_ostatky_products_list_productList read Fproduct;
  end;

  {  Tvvod_ostatky_products_list_product  }
  Tvvod_ostatky_products_list_product = class(TXmlSerializationObject)
  private
    Fki:Tkit_type;
    Fkitu:Tkitu_type;
    procedure Setki( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property ki:Tkit_type read Fki write Setki;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
  end;

implementation

  {  Tvvod_ostatky  }
procedure Tvvod_ostatky.Settrade_participant_inn(AValue: Tinn_type);
begin
  CheckStrMinSize('trade_participant_inn', AValue);
  CheckStrMaxSize('trade_participant_inn', AValue);
  Ftrade_participant_inn:=AValue;
  ModifiedProperty('trade_participant_inn');
end;

procedure Tvvod_ostatky.Setaction_id(AValue: Longint);
begin
  CheckFixedValue('action_id', AValue);
  Faction_id:=AValue;
  ModifiedProperty('action_id');
end;

procedure Tvvod_ostatky.Setversion(AValue: Longint);
begin
  CheckFixedValue('version', AValue);
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure Tvvod_ostatky.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('trade_participant_inn', 'trade_participant_inn', [xsaSimpleObject], '', 9, 12);
  P:=RegisterProperty('products_list', 'products_list', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='5';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='1';
end;

procedure Tvvod_ostatky.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproducts_list:=Tvvod_ostatky_products_list.Create;
end;

destructor Tvvod_ostatky.Destroy;
begin
  Fproducts_list.Free;
  inherited Destroy;
end;

constructor Tvvod_ostatky.Create;
begin
  inherited Create;
  action_id:=5;
  version:=1;
end;

  {  Tvvod_ostatky_element  }
procedure Tvvod_ostatky_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Tvvod_ostatky_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tvvod_ostatky_element.Destroy;
begin
  inherited Destroy;
end;

function Tvvod_ostatky_element.RootNodeName:string;
begin
  Result:='vvod_ostatky';
end;

constructor Tvvod_ostatky_element.Create;
begin
  inherited Create;
end;

  {  Tvvod_ostatky_products_list  }
procedure Tvvod_ostatky_products_list.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_ostatky_products_list.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_ostatky_products_list_productList.Create;
end;

destructor Tvvod_ostatky_products_list.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_ostatky_products_list.Create;
begin
  inherited Create;
end;

  {  Tvvod_ostatky_products_list_product  }
procedure Tvvod_ostatky_products_list_product.Setki(AValue: Tkit_type);
begin
  CheckStrMinSize('ki', AValue);
  CheckStrMaxSize('ki', AValue);
  Fki:=AValue;
  ModifiedProperty('ki');
end;

procedure Tvvod_ostatky_products_list_product.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Tvvod_ostatky_products_list_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('ki', 'ki', [xsaSimpleObject], '', 27, 44);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
end;

procedure Tvvod_ostatky_products_list_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tvvod_ostatky_products_list_product.Destroy;
begin
  inherited Destroy;
end;

constructor Tvvod_ostatky_products_list_product.Create;
begin
  inherited Create;
end;

end.