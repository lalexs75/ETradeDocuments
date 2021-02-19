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

unit vvod_individuals;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, LP_base_types, AbstractSerializationObjects;

type
  Taoguid = String;
  Thouseguid = String;
  Tflat = String;
  Tdoc_type = String;
type

  {  Forward declarations  }
  Tfias_address_type = class;
  Tvvod_individuals = class;
  Tvvod_individuals_element = class;
  Tvvod_individuals_products_list = class;
  Tvvod_individuals_products_list_product = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  Tvvod_individualsList = specialize GXMLSerializationObjectList<Tvvod_individuals>;
  Tvvod_individuals_elementList = specialize GXMLSerializationObjectList<Tvvod_individuals_element>;
  Tvvod_individuals_products_listList = specialize GXMLSerializationObjectList<Tvvod_individuals_products_list>;
  Tvvod_individuals_products_list_productList = specialize GXMLSerializationObjectList<Tvvod_individuals_products_list_product>;

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
    //Глобальный уникальный идентификатор домаОбязателен при наличии
    property houseguid:Thouseguid read Fhouseguid write Sethouseguid;
    //КвартираОбязателен при наличии
    property flat:Tflat read Fflat write Setflat;
  end;

  {  Tvvod_individuals  }
  //Ввод товаров в оборот, полученных от физических лиц
  Tvvod_individuals = class(TXmlSerializationObject)
  private
    Fdoc_type:Tdoc_type;
    Fparticipant_inn:TTRADE_PARTICIPANT_INN_type;
    Fproduct_receiving_date:Tdate_type;
    Fproducts_list:Tvvod_individuals_products_list;
    Faction_id:String;
    Fversion:Longint;
    procedure Setdoc_type( AValue:Tdoc_type);
    procedure Setparticipant_inn( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure Setproduct_receiving_date( AValue:Tdate_type);
    procedure Setaction_id( AValue:String);
    procedure Setversion( AValue:Longint);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property doc_type:Tdoc_type read Fdoc_type write Setdoc_type;
    //ИНН участника оборота
    property participant_inn:TTRADE_PARTICIPANT_INN_type read Fparticipant_inn write Setparticipant_inn;
    //Дата получения товара
    property product_receiving_date:Tdate_type read Fproduct_receiving_date write Setproduct_receiving_date;
    property products_list:Tvvod_individuals_products_list read Fproducts_list;
    property action_id:String read Faction_id write Setaction_id;
    property version:Longint read Fversion write Setversion;
  end;

  {  Tvvod_individuals_element  }
  Tvvod_individuals_element = class(Tvvod_individuals)
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

  {  Tvvod_individuals_products_list  }
  Tvvod_individuals_products_list = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_individuals_products_list_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property product:Tvvod_individuals_products_list_productList read Fproduct;
  end;

  {  Tvvod_individuals_products_list_product  }
  Tvvod_individuals_products_list_product = class(TXmlSerializationObject)
  private
    Fuit:Tgs1_uit_type;
    Fuitu:Tgs1_uitu_type;
    Fproduct_receiving_date:Tdate_type;
    procedure Setuit( AValue:Tgs1_uit_type);
    procedure Setuitu( AValue:Tgs1_uitu_type);
    procedure Setproduct_receiving_date( AValue:Tdate_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //УИТ
    property uit:Tgs1_uit_type read Fuit write Setuit;
    //УИТУ
    property uitu:Tgs1_uitu_type read Fuitu write Setuitu;
    //Дата получения товара
    property product_receiving_date:Tdate_type read Fproduct_receiving_date write Setproduct_receiving_date;
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

  {  Tvvod_individuals  }
procedure Tvvod_individuals.Setdoc_type(AValue: Tdoc_type);
begin
  CheckLockupValue('doc_type', AValue);
  Fdoc_type:=AValue;
  ModifiedProperty('doc_type');
end;

procedure Tvvod_individuals.Setparticipant_inn(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  CheckStrMinSize('participant_inn', AValue);
  CheckStrMaxSize('participant_inn', AValue);
  Fparticipant_inn:=AValue;
  ModifiedProperty('participant_inn');
end;

procedure Tvvod_individuals.Setproduct_receiving_date(AValue: Tdate_type);
begin
  CheckStrMinSize('product_receiving_date', AValue);
  CheckStrMaxSize('product_receiving_date', AValue);
  Fproduct_receiving_date:=AValue;
  ModifiedProperty('product_receiving_date');
end;

procedure Tvvod_individuals.Setaction_id(AValue: String);
begin
  CheckFixedValue('action_id', AValue);
  Faction_id:=AValue;
  ModifiedProperty('action_id');
end;

procedure Tvvod_individuals.Setversion(AValue: Longint);
begin
  CheckFixedValue('version', AValue);
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure Tvvod_individuals.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('doc_type', 'doc_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('vvod_individuals');
  P:=RegisterProperty('participant_inn', 'participant_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('product_receiving_date', 'product_receiving_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('products_list', 'products_list', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='5.3';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='2';
end;

procedure Tvvod_individuals.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproducts_list:=Tvvod_individuals_products_list.Create;
end;

destructor Tvvod_individuals.Destroy;
begin
  Fproducts_list.Free;
  inherited Destroy;
end;

constructor Tvvod_individuals.Create;
begin
  inherited Create;
  action_id:='5.3';
  version:=2;
end;

  {  Tvvod_individuals_element  }
procedure Tvvod_individuals_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Tvvod_individuals_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tvvod_individuals_element.Destroy;
begin
  inherited Destroy;
end;

function Tvvod_individuals_element.RootNodeName:string;
begin
  Result:='vvod_individuals';
end;

constructor Tvvod_individuals_element.Create;
begin
  inherited Create;
end;

  {  Tvvod_individuals_products_list  }
procedure Tvvod_individuals_products_list.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_individuals_products_list.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_individuals_products_list_productList.Create;
end;

destructor Tvvod_individuals_products_list.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_individuals_products_list.Create;
begin
  inherited Create;
end;

  {  Tvvod_individuals_products_list_product  }
procedure Tvvod_individuals_products_list_product.Setuit(AValue: Tgs1_uit_type);
begin
  CheckStrMinSize('uit', AValue);
  CheckStrMaxSize('uit', AValue);
  Fuit:=AValue;
  ModifiedProperty('uit');
end;

procedure Tvvod_individuals_products_list_product.Setuitu(AValue: Tgs1_uitu_type);
begin
  CheckStrMinSize('uitu', AValue);
  CheckStrMaxSize('uitu', AValue);
  Fuitu:=AValue;
  ModifiedProperty('uitu');
end;

procedure Tvvod_individuals_products_list_product.Setproduct_receiving_date(AValue: Tdate_type);
begin
  CheckStrMinSize('product_receiving_date', AValue);
  CheckStrMaxSize('product_receiving_date', AValue);
  Fproduct_receiving_date:=AValue;
  ModifiedProperty('product_receiving_date');
end;

procedure Tvvod_individuals_products_list_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('uit', 'uit', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('uitu', 'uitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('product_receiving_date', 'product_receiving_date', [xsaSimpleObject], '', 10, 10);
end;

procedure Tvvod_individuals_products_list_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tvvod_individuals_products_list_product.Destroy;
begin
  inherited Destroy;
end;

constructor Tvvod_individuals_products_list_product.Create;
begin
  inherited Create;
end;

end.
