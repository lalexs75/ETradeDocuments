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

unit packcode_transform;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, LP_base_types_v2;

type
  Taoguid = String;
  Thouseguid = String;
  Tflat = String;
type

  {  Forward declarations  }
  Tfias_address_type = class;
  Tchildren_products_list_type = class;
  Tchildren_products_list_type_product = class;
  Treaggregation = class;
  Treaggregation_element = class;
  Treaggregation_product_list = class;
  Treaggregation_product_list_packing_product = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  Tchildren_products_list_typeList = specialize GXMLSerializationObjectList<Tchildren_products_list_type>;
  Tchildren_products_list_type_productList = specialize GXMLSerializationObjectList<Tchildren_products_list_type_product>;
  TreaggregationList = specialize GXMLSerializationObjectList<Treaggregation>;
  Treaggregation_elementList = specialize GXMLSerializationObjectList<Treaggregation_element>;
  Treaggregation_product_listList = specialize GXMLSerializationObjectList<Treaggregation_product_list>;
  Treaggregation_product_list_packing_productList = specialize GXMLSerializationObjectList<Treaggregation_product_list_packing_product>;

  {  Tfias_address_type  }
  //Адрес в формате ФИАС
  Tfias_address_type = class(TXmlSerializationObject)
  private
    Faoguid:Taoguid;
    Fhouseguid:Thouseguid;
    Fflat:Tflat;
    procedure Setaoguid( AValue:Taoguid);
    procedure Sethouseguid( AValue:Thouseguid);
    procedure Setflat( AValue:Tflat);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Глобальный уникальный идентификатор адресного объекта
    property aoguid:Taoguid read Faoguid write Setaoguid;
    //Глобальный уникальный идентификатор дома. Обязателен при наличии
    property houseguid:Thouseguid read Fhouseguid write Sethouseguid;
    //Квартира. Обязателен при наличии
    property flat:Tflat read Fflat write Setflat;
  end;

  {  Tchildren_products_list_type  }
  Tchildren_products_list_type = class(TXmlSerializationObject)
  private
    Fproduct:Tchildren_products_list_type_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Список товаров в составе транспортной упаковки
    property product:Tchildren_products_list_type_productList read Fproduct;
  end;

  {  Tchildren_products_list_type_product  }
  Tchildren_products_list_type_product = class(TXmlSerializationObject)
  private
    Fkit:Tkit_type;
    Fkitu:Tkitu_type;
    Faccept_type:Boolean;
    Fchildren_products_list:Tchildren_products_list_type;
    procedure Setkit( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
    procedure Setaccept_type( AValue:Boolean);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИ
    property kit:Tkit_type read Fkit write Setkit;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Товар принят
    property accept_type:Boolean read Faccept_type write Setaccept_type;
    //Список товаров в составе транспортной упаковки
    property children_products_list:Tchildren_products_list_type read Fchildren_products_list;
  end;

  {  Treaggregation  }
  //Трансформация транспортных упаковок
  Treaggregation = class(TXmlSerializationObject)
  private
    Ftrade_participant_inn:Tinn_type;
    Freaggregation_type:Treaggregation_type_type;
    Freaggregating_kitu:Tkitu_type;
    Fproduct_list:Treaggregation_product_list;
    Faction_id:Longint;
    Fversion:String;
    procedure Settrade_participant_inn( AValue:Tinn_type);
    procedure Setreaggregation_type( AValue:Treaggregation_type_type);
    procedure Setreaggregating_kitu( AValue:Tkitu_type);
    procedure Setaction_id( AValue:Longint);
    procedure Setversion( AValue:String);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //ИНН участника оборота
    property trade_participant_inn:Tinn_type read Ftrade_participant_inn write Settrade_participant_inn;
    //Тип трансформации
    //Изъятие - REMOVING
    //Добавление - ADDING
    property reaggregation_type:Treaggregation_type_type read Freaggregation_type write Setreaggregation_type;
    //Код идентификации трансформируемой транспортной упаковки (КИТУ)
    property reaggregating_kitu:Tkitu_type read Freaggregating_kitu write Setreaggregating_kitu;
    //Список транспортных упаковок или товаров
    property product_list:Treaggregation_product_list read Fproduct_list;
    property action_id:Longint read Faction_id write Setaction_id;
    property version:String read Fversion write Setversion;
  end;

  {  Treaggregation_element  }
  Treaggregation_element = class(Treaggregation)
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

  {  Treaggregation_product_list  }
  Treaggregation_product_list = class(TXmlSerializationObject)
  private
    Fpacking_product:Treaggregation_product_list_packing_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Транспортная упаковка или товар
    property packing_product:Treaggregation_product_list_packing_productList read Fpacking_product;
  end;

  {  Treaggregation_product_list_packing_product  }
  Treaggregation_product_list_packing_product = class(TXmlSerializationObject)
  private
    Fkit:Tkit_type;
    Fkitu:Tkitu_type;
    procedure Setkit( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИТ
    property kit:Tkit_type read Fkit write Setkit;
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
  end;

implementation

  {  Tfias_address_type  }
procedure Tfias_address_type.Setaoguid(AValue: Taoguid);
begin
  CheckStrMinSize('aoguid', AValue);
  CheckStrMaxSize('aoguid', AValue);
  Faoguid:=AValue;
  ModifiedProperty('aoguid');
end;

procedure Tfias_address_type.Sethouseguid(AValue: Thouseguid);
begin
  CheckStrMinSize('houseguid', AValue);
  CheckStrMaxSize('houseguid', AValue);
  Fhouseguid:=AValue;
  ModifiedProperty('houseguid');
end;

procedure Tfias_address_type.Setflat(AValue: Tflat);
begin
  CheckStrMinSize('flat', AValue);
  CheckStrMaxSize('flat', AValue);
  Fflat:=AValue;
  ModifiedProperty('flat');
end;

procedure Tfias_address_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('aoguid', 'aoguid', [xsaSimpleObject], '', 36, 36);
  P:=RegisterProperty('houseguid', 'houseguid', [xsaSimpleObject], '', 36, 36);
  P:=RegisterProperty('flat', 'flat', [xsaSimpleObject], '', 1, 20);
end;

procedure Tfias_address_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tfias_address_type.Destroy;
begin
  inherited Destroy;
end;

constructor Tfias_address_type.Create;
begin
  inherited Create;
end;

  {  Tchildren_products_list_type  }
procedure Tchildren_products_list_type.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tchildren_products_list_type.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tchildren_products_list_type_productList.Create;
end;

destructor Tchildren_products_list_type.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tchildren_products_list_type.Create;
begin
  inherited Create;
end;

  {  Tchildren_products_list_type_product  }
procedure Tchildren_products_list_type_product.Setkit(AValue: Tkit_type);
begin
  CheckStrMinSize('kit', AValue);
  CheckStrMaxSize('kit', AValue);
  Fkit:=AValue;
  ModifiedProperty('kit');
end;

procedure Tchildren_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Tchildren_products_list_type_product.Setaccept_type(AValue: Boolean);
begin
  Faccept_type:=AValue;
  ModifiedProperty('accept_type');
end;

procedure Tchildren_products_list_type_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('kit', 'kit', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('accept_type', 'accept_type', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('children_products_list', 'children_products_list', [], '', -1, -1);
end;

procedure Tchildren_products_list_type_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fchildren_products_list:=Tchildren_products_list_type.Create;
end;

destructor Tchildren_products_list_type_product.Destroy;
begin
  Fchildren_products_list.Free;
  inherited Destroy;
end;

constructor Tchildren_products_list_type_product.Create;
begin
  inherited Create;
end;

  {  Treaggregation  }
procedure Treaggregation.Settrade_participant_inn(AValue: Tinn_type);
begin
  CheckStrMinSize('trade_participant_inn', AValue);
  CheckStrMaxSize('trade_participant_inn', AValue);
  Ftrade_participant_inn:=AValue;
  ModifiedProperty('trade_participant_inn');
end;

procedure Treaggregation.Setreaggregation_type(AValue: Treaggregation_type_type);
begin
  CheckLockupValue('reaggregation_type', AValue);
  Freaggregation_type:=AValue;
  ModifiedProperty('reaggregation_type');
end;

procedure Treaggregation.Setreaggregating_kitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('reaggregating_kitu', AValue);
  CheckStrMaxSize('reaggregating_kitu', AValue);
  Freaggregating_kitu:=AValue;
  ModifiedProperty('reaggregating_kitu');
end;

procedure Treaggregation.Setaction_id(AValue: Longint);
begin
  CheckFixedValue('action_id', AValue);
  Faction_id:=AValue;
  ModifiedProperty('action_id');
end;

procedure Treaggregation.Setversion(AValue: String);
begin
  CheckFixedValue('version', AValue);
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure Treaggregation.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('trade_participant_inn', 'trade_participant_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('reaggregation_type', 'reaggregation_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('REMOVING');
    P.ValidList.Add('ADDING');
  P:=RegisterProperty('reaggregating_kitu', 'reaggregating_kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('product_list', 'product_list', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='32';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='2';
end;

procedure Treaggregation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct_list:=Treaggregation_product_list.Create;
end;

destructor Treaggregation.Destroy;
begin
  Fproduct_list.Free;
  inherited Destroy;
end;

constructor Treaggregation.Create;
begin
  inherited Create;
  action_id:=32;
  version:='2';
end;

  {  Treaggregation_element  }
procedure Treaggregation_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Treaggregation_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Treaggregation_element.Destroy;
begin
  inherited Destroy;
end;

function Treaggregation_element.RootNodeName:string;
begin
  Result:='reaggregation';
end;

constructor Treaggregation_element.Create;
begin
  inherited Create;
end;

  {  Treaggregation_product_list  }
procedure Treaggregation_product_list.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('packing_product', 'packing_product', [], '', -1, -1);
end;

procedure Treaggregation_product_list.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fpacking_product:=Treaggregation_product_list_packing_productList.Create;
end;

destructor Treaggregation_product_list.Destroy;
begin
  Fpacking_product.Free;
  inherited Destroy;
end;

constructor Treaggregation_product_list.Create;
begin
  inherited Create;
end;

  {  Treaggregation_product_list_packing_product  }
procedure Treaggregation_product_list_packing_product.Setkit(AValue: Tkit_type);
begin
  CheckStrMinSize('kit', AValue);
  CheckStrMaxSize('kit', AValue);
  Fkit:=AValue;
  ModifiedProperty('kit');
end;

procedure Treaggregation_product_list_packing_product.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Treaggregation_product_list_packing_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('kit', 'kit', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
end;

procedure Treaggregation_product_list_packing_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Treaggregation_product_list_packing_product.Destroy;
begin
  inherited Destroy;
end;

constructor Treaggregation_product_list_packing_product.Create;
begin
  inherited Create;
end;

end.
