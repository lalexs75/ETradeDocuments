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

unit withdrawal;

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
  Twithdrawal = class;
  Twithdrawal_element = class;
  Twithdrawal_products_list = class;
  Twithdrawal_products_list_product = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  Tchildren_products_list_typeList = specialize GXMLSerializationObjectList<Tchildren_products_list_type>;
  Tchildren_products_list_type_productList = specialize GXMLSerializationObjectList<Tchildren_products_list_type_product>;
  TwithdrawalList = specialize GXMLSerializationObjectList<Twithdrawal>;
  Twithdrawal_elementList = specialize GXMLSerializationObjectList<Twithdrawal_element>;
  Twithdrawal_products_listList = specialize GXMLSerializationObjectList<Twithdrawal_products_list>;
  Twithdrawal_products_list_productList = specialize GXMLSerializationObjectList<Twithdrawal_products_list_product>;

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

  {  Twithdrawal  }
  //Вывод из оборота
  Twithdrawal = class(TXmlSerializationObject)
  private
    Ftrade_participant_inn:Tinn_type;
    Fwithdrawal_type:Twithdrawal_type;
    Fwithdrawal_date:Tdate_type;
    Fprimary_document_type:Tprimary_document_name_type;
    Fprimary_document_number:Tstring255_type;
    Fprimary_document_date:Tdate_type;
    Fproducts_list:Twithdrawal_products_list;
    Faction_id:Longint;
    Fversion:Double;
    procedure Settrade_participant_inn( AValue:Tinn_type);
    procedure Setwithdrawal_type( AValue:Twithdrawal_type);
    procedure Setwithdrawal_date( AValue:Tdate_type);
    procedure Setprimary_document_type( AValue:Tprimary_document_name_type);
    procedure Setprimary_document_number( AValue:Tstring255_type);
    procedure Setprimary_document_date( AValue:Tdate_type);
    procedure Setaction_id( AValue:Longint);
    procedure Setversion( AValue:Double);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //ИНН участника оборота
    property trade_participant_inn:Tinn_type read Ftrade_participant_inn write Settrade_participant_inn;
    //Причина вывода из оборота (розничная продажа - RETAIL)
    property withdrawal_type:Twithdrawal_type read Fwithdrawal_type write Setwithdrawal_type;
    //Дата вывода из оборота
    property withdrawal_date:Tdate_type read Fwithdrawal_date write Setwithdrawal_date;
    //Наименование первичного документа
    property primary_document_type:Tprimary_document_name_type read Fprimary_document_type write Setprimary_document_type;
    //Номер первичного документа
    property primary_document_number:Tstring255_type read Fprimary_document_number write Setprimary_document_number;
    //Дата первичного документа
    property primary_document_date:Tdate_type read Fprimary_document_date write Setprimary_document_date;
    //Параметры товаров
    property products_list:Twithdrawal_products_list read Fproducts_list;
    property action_id:Longint read Faction_id write Setaction_id;
    property version:Double read Fversion write Setversion;
  end;

  {  Twithdrawal_element  }
  Twithdrawal_element = class(Twithdrawal)
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

  {  Twithdrawal_products_list  }
  Twithdrawal_products_list = class(TXmlSerializationObject)
  private
    Fproduct:Twithdrawal_products_list_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Параметры товара
    property product:Twithdrawal_products_list_productList read Fproduct;
  end;

  {  Twithdrawal_products_list_product  }
  Twithdrawal_products_list_product = class(TXmlSerializationObject)
  private
    Fkit:Tkit_type;
    Fcost:Tprice_type;
    Fvat_value:Tprice_type;
    procedure Setkit( AValue:Tkit_type);
    procedure Setcost( AValue:Tprice_type);
    procedure Setvat_value( AValue:Tprice_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //КИТ
    property kit:Tkit_type read Fkit write Setkit;
    //Цена за единицу
    property cost:Tprice_type read Fcost write Setcost;
    //Сумма НДС
    property vat_value:Tprice_type read Fvat_value write Setvat_value;
  end;

implementation

  {  Tfias_address_type  }
procedure Tfias_address_type.Setaoguid(AValue: Taoguid);
begin
  Faoguid:=AValue;
  CheckStrMinSize('aoguid', AValue);
  CheckStrMaxSize('aoguid', AValue);
  ModifiedProperty('aoguid');
end;

procedure Tfias_address_type.Sethouseguid(AValue: Thouseguid);
begin
  Fhouseguid:=AValue;
  CheckStrMinSize('houseguid', AValue);
  CheckStrMaxSize('houseguid', AValue);
  ModifiedProperty('houseguid');
end;

procedure Tfias_address_type.Setflat(AValue: Tflat);
begin
  Fflat:=AValue;
  CheckStrMinSize('flat', AValue);
  CheckStrMaxSize('flat', AValue);
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
  Fkit:=AValue;
  CheckStrMinSize('kit', AValue);
  CheckStrMaxSize('kit', AValue);
  ModifiedProperty('kit');
end;

procedure Tchildren_products_list_type_product.Setkitu(AValue: Tkitu_type);
begin
  Fkitu:=AValue;
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
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

  {  Twithdrawal  }
procedure Twithdrawal.Settrade_participant_inn(AValue: Tinn_type);
begin
  Ftrade_participant_inn:=AValue;
  CheckStrMinSize('trade_participant_inn', AValue);
  CheckStrMaxSize('trade_participant_inn', AValue);
  ModifiedProperty('trade_participant_inn');
end;

procedure Twithdrawal.Setwithdrawal_type(AValue: Twithdrawal_type);
begin
  Fwithdrawal_type:=AValue;
  CheckLockupValue('withdrawal_type', AValue);
  ModifiedProperty('withdrawal_type');
end;

procedure Twithdrawal.Setwithdrawal_date(AValue: Tdate_type);
begin
  Fwithdrawal_date:=AValue;
  CheckStrMinSize('withdrawal_date', AValue);
  CheckStrMaxSize('withdrawal_date', AValue);
  ModifiedProperty('withdrawal_date');
end;

procedure Twithdrawal.Setprimary_document_type(AValue: Tprimary_document_name_type);
begin
  Fprimary_document_type:=AValue;
  CheckLockupValue('primary_document_type', AValue);
  ModifiedProperty('primary_document_type');
end;

procedure Twithdrawal.Setprimary_document_number(AValue: Tstring255_type);
begin
  Fprimary_document_number:=AValue;
  CheckStrMinSize('primary_document_number', AValue);
  CheckStrMaxSize('primary_document_number', AValue);
  ModifiedProperty('primary_document_number');
end;

procedure Twithdrawal.Setprimary_document_date(AValue: Tdate_type);
begin
  Fprimary_document_date:=AValue;
  CheckStrMinSize('primary_document_date', AValue);
  CheckStrMaxSize('primary_document_date', AValue);
  ModifiedProperty('primary_document_date');
end;

procedure Twithdrawal.Setaction_id(AValue: Longint);
begin
  Faction_id:=AValue;
  CheckFixedValue('action_id', AValue);
  ModifiedProperty('action_id');
end;

procedure Twithdrawal.Setversion(AValue: Double);
begin
  Fversion:=AValue;
  CheckFixedValue('version', AValue);
  ModifiedProperty('version');
end;

procedure Twithdrawal.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('trade_participant_inn', 'trade_participant_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('withdrawal_type', 'withdrawal_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('RETAIL');
    P.ValidList.Add('EEC_EXPORT');
    P.ValidList.Add('BEYOND_EEC_EXPORT');
    P.ValidList.Add('RETURN');
    P.ValidList.Add('REMOTE_SALE');
    P.ValidList.Add('DAMAGE_LOSS');
    P.ValidList.Add('DESTRUCTION');
    P.ValidList.Add('CONFISCATION');
    P.ValidList.Add('LIQUIDATION');
  P:=RegisterProperty('withdrawal_date', 'withdrawal_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('primary_document_type', 'primary_document_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('RECEIPT');
    P.ValidList.Add('CORRECTION_RECEIPT');
    P.ValidList.Add('SALES_RECEIPT');
    P.ValidList.Add('CONSIGNMENT_NOTE');
    P.ValidList.Add('UTD');
    P.ValidList.Add('CUSTOMS_DECLARATION');
    P.ValidList.Add('DESTRUCTION_ACT');
    P.ValidList.Add('OTHER');
  P:=RegisterProperty('primary_document_number', 'primary_document_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('primary_document_date', 'primary_document_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('products_list', 'products_list', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='15';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='2';
end;

procedure Twithdrawal.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproducts_list:=Twithdrawal_products_list.Create;
end;

destructor Twithdrawal.Destroy;
begin
  Fproducts_list.Free;
  inherited Destroy;
end;

constructor Twithdrawal.Create;
begin
  inherited Create;
  action_id:=15;
  version:=2;
end;

  {  Twithdrawal_element  }
procedure Twithdrawal_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Twithdrawal_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Twithdrawal_element.Destroy;
begin
  inherited Destroy;
end;

function Twithdrawal_element.RootNodeName:string;
begin
  Result:='withdrawal';
end;

constructor Twithdrawal_element.Create;
begin
  inherited Create;
end;

  {  Twithdrawal_products_list  }
procedure Twithdrawal_products_list.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Twithdrawal_products_list.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Twithdrawal_products_list_productList.Create;
end;

destructor Twithdrawal_products_list.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Twithdrawal_products_list.Create;
begin
  inherited Create;
end;

  {  Twithdrawal_products_list_product  }
procedure Twithdrawal_products_list_product.Setkit(AValue: Tkit_type);
begin
  Fkit:=AValue;
  CheckStrMinSize('kit', AValue);
  CheckStrMaxSize('kit', AValue);
  ModifiedProperty('kit');
end;

procedure Twithdrawal_products_list_product.Setcost(AValue: Tprice_type);
begin
  Fcost:=AValue;
  CheckMinInclusiveValue('cost', AValue);
  ModifiedProperty('cost');
end;

procedure Twithdrawal_products_list_product.Setvat_value(AValue: Tprice_type);
begin
  Fvat_value:=AValue;
  CheckMinInclusiveValue('vat_value', AValue);
  ModifiedProperty('vat_value');
end;

procedure Twithdrawal_products_list_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('kit', 'kit', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('cost', 'cost', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('vat_value', 'vat_value', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
end;

procedure Twithdrawal_products_list_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Twithdrawal_products_list_product.Destroy;
begin
  inherited Destroy;
end;

constructor Twithdrawal_products_list_product.Create;
begin
  inherited Create;
end;

end.