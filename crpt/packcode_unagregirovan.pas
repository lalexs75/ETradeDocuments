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

unit packcode_unagregirovan;

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
  Tdisaggregation = class;
  Tdisaggregation_element = class;
  Tdisaggregation_packings_list = class;
  Tdisaggregation_packings_list_packing = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  Tchildren_products_list_typeList = specialize GXMLSerializationObjectList<Tchildren_products_list_type>;
  Tchildren_products_list_type_productList = specialize GXMLSerializationObjectList<Tchildren_products_list_type_product>;
  TdisaggregationList = specialize GXMLSerializationObjectList<Tdisaggregation>;
  Tdisaggregation_elementList = specialize GXMLSerializationObjectList<Tdisaggregation_element>;
  Tdisaggregation_packings_listList = specialize GXMLSerializationObjectList<Tdisaggregation_packings_list>;
  Tdisaggregation_packings_list_packingList = specialize GXMLSerializationObjectList<Tdisaggregation_packings_list_packing>;

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

  {  Tdisaggregation  }
  //Расформирование транспортных упаковок
  Tdisaggregation = class(TXmlSerializationObject)
  private
    Ftrade_participant_inn:Tinn_type;
    Fpackings_list:Tdisaggregation_packings_list;
    Faction_id:Longint;
    Fversion:String;
    procedure Settrade_participant_inn( AValue:Tinn_type);
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
    //Список транспортных упаковок
    property packings_list:Tdisaggregation_packings_list read Fpackings_list;
    property action_id:Longint read Faction_id write Setaction_id;
    property version:String read Fversion write Setversion;
  end;

  {  Tdisaggregation_element  }
  Tdisaggregation_element = class(Tdisaggregation)
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

  {  Tdisaggregation_packings_list  }
  Tdisaggregation_packings_list = class(TXmlSerializationObject)
  private
    Fpacking:Tdisaggregation_packings_list_packingList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Транспортная упаковка
    property packing:Tdisaggregation_packings_list_packingList read Fpacking;
  end;

  {  Tdisaggregation_packings_list_packing  }
  Tdisaggregation_packings_list_packing = class(TXmlSerializationObject)
  private
    Fkitu:Tkitu_type;
    procedure Setkitu( AValue:Tkitu_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
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

  {  Tdisaggregation  }
procedure Tdisaggregation.Settrade_participant_inn(AValue: Tinn_type);
begin
  CheckStrMinSize('trade_participant_inn', AValue);
  CheckStrMaxSize('trade_participant_inn', AValue);
  Ftrade_participant_inn:=AValue;
  ModifiedProperty('trade_participant_inn');
end;

procedure Tdisaggregation.Setaction_id(AValue: Longint);
begin
  CheckFixedValue('action_id', AValue);
  Faction_id:=AValue;
  ModifiedProperty('action_id');
end;

procedure Tdisaggregation.Setversion(AValue: String);
begin
  CheckFixedValue('version', AValue);
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure Tdisaggregation.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('trade_participant_inn', 'trade_participant_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('packings_list', 'packings_list', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='31';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='2';
end;

procedure Tdisaggregation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fpackings_list:=Tdisaggregation_packings_list.Create;
end;

destructor Tdisaggregation.Destroy;
begin
  Fpackings_list.Free;
  inherited Destroy;
end;

constructor Tdisaggregation.Create;
begin
  inherited Create;
  action_id:=31;
  version:='2';
end;

  {  Tdisaggregation_element  }
procedure Tdisaggregation_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Tdisaggregation_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tdisaggregation_element.Destroy;
begin
  inherited Destroy;
end;

function Tdisaggregation_element.RootNodeName:string;
begin
  Result:='disaggregation';
end;

constructor Tdisaggregation_element.Create;
begin
  inherited Create;
end;

  {  Tdisaggregation_packings_list  }
procedure Tdisaggregation_packings_list.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('packing', 'packing', [], '', -1, -1);
end;

procedure Tdisaggregation_packings_list.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fpacking:=Tdisaggregation_packings_list_packingList.Create;
end;

destructor Tdisaggregation_packings_list.Destroy;
begin
  Fpacking.Free;
  inherited Destroy;
end;

constructor Tdisaggregation_packings_list.Create;
begin
  inherited Create;
end;

  {  Tdisaggregation_packings_list_packing  }
procedure Tdisaggregation_packings_list_packing.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Tdisaggregation_packings_list_packing.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
end;

procedure Tdisaggregation_packings_list_packing.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tdisaggregation_packings_list_packing.Destroy;
begin
  inherited Destroy;
end;

constructor Tdisaggregation_packings_list_packing.Create;
begin
  inherited Create;
end;

end.
