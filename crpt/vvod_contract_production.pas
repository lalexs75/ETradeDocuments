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

unit vvod_contract_production;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, LP_base_types;

type
  Taoguid = String;
  Thouseguid = String;
  Tflat = String;
  TDocType = String;
  Tproduction_order = String;
  Tcertificate_doc_number = String;
type

  {  Forward declarations  }
  Tfias_address_type = class;
  Tvvod_contract_production = class;
  Tvvod_contract_production_element = class;
  Tvvod_contract_production_products_list = class;
  Tvvod_contract_production_products_list_product = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  Tvvod_contract_productionList = specialize GXMLSerializationObjectList<Tvvod_contract_production>;
  Tvvod_contract_production_elementList = specialize GXMLSerializationObjectList<Tvvod_contract_production_element>;
  Tvvod_contract_production_products_listList = specialize GXMLSerializationObjectList<Tvvod_contract_production_products_list>;
  Tvvod_contract_production_products_list_productList = specialize GXMLSerializationObjectList<Tvvod_contract_production_products_list_product>;

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

  {  Tvvod_contract_production  }
  //Ввод товаров в оборот, произведенного сторонней организацией
  Tvvod_contract_production = class(TXmlSerializationObject)
  private
    FDocType:TDocType;
    Fproducer_inn:TTRADE_PARTICIPANT_INN_type;
    Fowner_inn:TTRADE_PARTICIPANT_INN_type;
    Fproduction_date:Tdate_type;
    Fproduction_order:Tproduction_order;
    Fproducts_list:Tvvod_contract_production_products_list;
    Faction_id:String;
    Fversion:Longint;
    procedure SetDocType( AValue:TDocType);
    procedure Setproducer_inn( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure Setowner_inn( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure Setproduction_date( AValue:Tdate_type);
    procedure Setproduction_order( AValue:Tproduction_order);
    procedure Setaction_id( AValue:String);
    procedure Setversion( AValue:Longint);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property DocType:TDocType read FDocType write SetDocType;
    //ИНН производителя или импортера товара
    property producer_inn:TTRADE_PARTICIPANT_INN_type read Fproducer_inn write Setproducer_inn;
    //ИНН собственника товара
    property owner_inn:TTRADE_PARTICIPANT_INN_type read Fowner_inn write Setowner_inn;
    //Дата производства
    property production_date:Tdate_type read Fproduction_date write Setproduction_date;
    //Тип производственного заказа
    //Собственное производство - OWN_PRODUCTION
    //Производство товара по договору - CONTRACT_PRODUCTION
    property production_order:Tproduction_order read Fproduction_order write Setproduction_order;
    property products_list:Tvvod_contract_production_products_list read Fproducts_list;
    property action_id:String read Faction_id write Setaction_id;
    property version:Longint read Fversion write Setversion;
  end;

  {  Tvvod_contract_production_element  }
  Tvvod_contract_production_element = class(Tvvod_contract_production)
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

  {  Tvvod_contract_production_products_list  }
  Tvvod_contract_production_products_list = class(TXmlSerializationObject)
  private
    Fproduct:Tvvod_contract_production_products_list_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property product:Tvvod_contract_production_products_list_productList read Fproduct;
  end;

  {  Tvvod_contract_production_products_list_product  }
  Tvvod_contract_production_products_list_product = class(TXmlSerializationObject)
  private
    Fuit:Tgs1_uit_type;
    Fuitu:Tgs1_uitu_type;
    Ftnved_code:Ttnved_code_type;
    Fproduction_date:Tdate_type;
    Fcertificate_doc_number:Tcertificate_doc_number;
    Fcertificate_doc_date:Tdate_type;
    procedure Setuit( AValue:Tgs1_uit_type);
    procedure Setuitu( AValue:Tgs1_uitu_type);
    procedure Settnved_code( AValue:Ttnved_code_type);
    procedure Setproduction_date( AValue:Tdate_type);
    procedure Setcertificate_doc_number( AValue:Tcertificate_doc_number);
    procedure Setcertificate_doc_date( AValue:Tdate_type);
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
    //Код ТН ВЭД ЕАС
    property tnved_code:Ttnved_code_type read Ftnved_code write Settnved_code;
    //Дата производства
    property production_date:Tdate_type read Fproduction_date write Setproduction_date;
    //Номер документа обязательной сертификации
    property certificate_doc_number:Tcertificate_doc_number read Fcertificate_doc_number write Setcertificate_doc_number;
    //Дата документа обязательной сертификации
    property certificate_doc_date:Tdate_type read Fcertificate_doc_date write Setcertificate_doc_date;
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

  {  Tvvod_contract_production  }
procedure Tvvod_contract_production.SetDocType(AValue: TDocType);
begin
  CheckLockupValue('DocType', AValue);
  FDocType:=AValue;
  ModifiedProperty('DocType');
end;

procedure Tvvod_contract_production.Setproducer_inn(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  CheckStrMinSize('producer_inn', AValue);
  CheckStrMaxSize('producer_inn', AValue);
  Fproducer_inn:=AValue;
  ModifiedProperty('producer_inn');
end;

procedure Tvvod_contract_production.Setowner_inn(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  CheckStrMinSize('owner_inn', AValue);
  CheckStrMaxSize('owner_inn', AValue);
  Fowner_inn:=AValue;
  ModifiedProperty('owner_inn');
end;

procedure Tvvod_contract_production.Setproduction_date(AValue: Tdate_type);
begin
  CheckStrMinSize('production_date', AValue);
  CheckStrMaxSize('production_date', AValue);
  Fproduction_date:=AValue;
  ModifiedProperty('production_date');
end;

procedure Tvvod_contract_production.Setproduction_order(AValue: Tproduction_order);
begin
  CheckLockupValue('production_order', AValue);
  Fproduction_order:=AValue;
  ModifiedProperty('production_order');
end;

procedure Tvvod_contract_production.Setaction_id(AValue: String);
begin
  CheckFixedValue('action_id', AValue);
  Faction_id:=AValue;
  ModifiedProperty('action_id');
end;

procedure Tvvod_contract_production.Setversion(AValue: Longint);
begin
  CheckFixedValue('version', AValue);
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure Tvvod_contract_production.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('DocType', 'DocType', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('vvod_contract_production');
  P:=RegisterProperty('producer_inn', 'producer_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('owner_inn', 'owner_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('production_date', 'production_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('production_order', 'production_order', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('OWN_PRODUCTION');
    P.ValidList.Add('CONTRACT_PRODUCTION');
  P:=RegisterProperty('products_list', 'products_list', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='5.2';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='2';
end;

procedure Tvvod_contract_production.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproducts_list:=Tvvod_contract_production_products_list.Create;
end;

destructor Tvvod_contract_production.Destroy;
begin
  Fproducts_list.Free;
  inherited Destroy;
end;

constructor Tvvod_contract_production.Create;
begin
  inherited Create;
  action_id:='5.2';
  version:=2;
end;

  {  Tvvod_contract_production_element  }
procedure Tvvod_contract_production_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Tvvod_contract_production_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tvvod_contract_production_element.Destroy;
begin
  inherited Destroy;
end;

function Tvvod_contract_production_element.RootNodeName:string;
begin
  Result:='vvod_contract_production';
end;

constructor Tvvod_contract_production_element.Create;
begin
  inherited Create;
end;

  {  Tvvod_contract_production_products_list  }
procedure Tvvod_contract_production_products_list.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tvvod_contract_production_products_list.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tvvod_contract_production_products_list_productList.Create;
end;

destructor Tvvod_contract_production_products_list.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tvvod_contract_production_products_list.Create;
begin
  inherited Create;
end;

  {  Tvvod_contract_production_products_list_product  }
procedure Tvvod_contract_production_products_list_product.Setuit(AValue: Tgs1_uit_type);
begin
  CheckStrMinSize('uit', AValue);
  CheckStrMaxSize('uit', AValue);
  Fuit:=AValue;
  ModifiedProperty('uit');
end;

procedure Tvvod_contract_production_products_list_product.Setuitu(AValue: Tgs1_uitu_type);
begin
  CheckStrMinSize('uitu', AValue);
  CheckStrMaxSize('uitu', AValue);
  Fuitu:=AValue;
  ModifiedProperty('uitu');
end;

procedure Tvvod_contract_production_products_list_product.Settnved_code(AValue: Ttnved_code_type);
begin
  CheckMinInclusiveValue('tnved_code', AValue);
  CheckMaxInclusiveValue('tnved_code', AValue);
  Ftnved_code:=AValue;
  ModifiedProperty('tnved_code');
end;

procedure Tvvod_contract_production_products_list_product.Setproduction_date(AValue: Tdate_type);
begin
  CheckStrMinSize('production_date', AValue);
  CheckStrMaxSize('production_date', AValue);
  Fproduction_date:=AValue;
  ModifiedProperty('production_date');
end;

procedure Tvvod_contract_production_products_list_product.Setcertificate_doc_number(AValue: Tcertificate_doc_number);
begin
  CheckStrMinSize('certificate_doc_number', AValue);
  CheckStrMaxSize('certificate_doc_number', AValue);
  Fcertificate_doc_number:=AValue;
  ModifiedProperty('certificate_doc_number');
end;

procedure Tvvod_contract_production_products_list_product.Setcertificate_doc_date(AValue: Tdate_type);
begin
  CheckStrMinSize('certificate_doc_date', AValue);
  CheckStrMaxSize('certificate_doc_date', AValue);
  Fcertificate_doc_date:=AValue;
  ModifiedProperty('certificate_doc_date');
end;

procedure Tvvod_contract_production_products_list_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('uit', 'uit', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('uitu', 'uitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('tnved_code', 'tnved_code', [xsaSimpleObject], '', -1, -1);
    P.minInclusiveInt:=6401000000;
    P.maxInclusiveInt:=6405999999;
  P:=RegisterProperty('production_date', 'production_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('certificate_doc_number', 'certificate_doc_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('certificate_doc_date', 'certificate_doc_date', [xsaSimpleObject], '', 10, 10);
end;

procedure Tvvod_contract_production_products_list_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tvvod_contract_production_products_list_product.Destroy;
begin
  inherited Destroy;
end;

constructor Tvvod_contract_production_products_list_product.Create;
begin
  inherited Create;
end;

end.
