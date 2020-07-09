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

unit shipment;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, LP_base_types_v2, AbstractSerializationObjects;

type

  {  Forward declarations  }
  Tshipment = class;
  Tshipment_element = class;
  Tshipment_products_list = class;
  Tshipment_products_list_product = class;

  {  Generic classes for collections  }
  TshipmentList = specialize GXMLSerializationObjectList<Tshipment>;
  Tshipment_elementList = specialize GXMLSerializationObjectList<Tshipment_element>;
  Tshipment_products_listList = specialize GXMLSerializationObjectList<Tshipment_products_list>;
  Tshipment_products_list_productList = specialize GXMLSerializationObjectList<Tshipment_products_list_product>;

  {  Tshipment  }
  //Отгрузка
  Tshipment = class(TXmlSerializationObject)
  private
    Ftrade_participant_inn_sender:Tinn_type;
    Ftrade_participant_inn_receiver:Tinn_type;
    Ftrade_participant_inn_owner:Tinn_type;
    Ftransfer_date:Tdate_type;
    Fmove_document_number:Tstring255_type;
    Fmove_document_date:Tdate_type;
    Fturnover_type:Tturnover_enum_type;
    Fwithdrawal_type:Twithdrawal_shipment_type;
    Fwithdrawal_date:Tdate_type;
    Fst_contract_id:Tstring255_type;
    Fto_not_participant:Boolean;
    Fproducts_list:Tshipment_products_list;
    Faction_id:Longint;
    Fversion:Double;
    procedure Settrade_participant_inn_sender( AValue:Tinn_type);
    procedure Settrade_participant_inn_receiver( AValue:Tinn_type);
    procedure Settrade_participant_inn_owner( AValue:Tinn_type);
    procedure Settransfer_date( AValue:Tdate_type);
    procedure Setmove_document_number( AValue:Tstring255_type);
    procedure Setmove_document_date( AValue:Tdate_type);
    procedure Setturnover_type( AValue:Tturnover_enum_type);
    procedure Setwithdrawal_type( AValue:Twithdrawal_shipment_type);
    procedure Setwithdrawal_date( AValue:Tdate_type);
    procedure Setst_contract_id( AValue:Tstring255_type);
    procedure Setto_not_participant( AValue:Boolean);
    procedure Setaction_id( AValue:Longint);
    procedure Setversion( AValue:Double);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //ИНН отправителя
    property trade_participant_inn_sender:Tinn_type read Ftrade_participant_inn_sender write Settrade_participant_inn_sender;
    //ИНН получателя
    property trade_participant_inn_receiver:Tinn_type read Ftrade_participant_inn_receiver write Settrade_participant_inn_receiver;
    //ИНН собственника
    property trade_participant_inn_owner:Tinn_type read Ftrade_participant_inn_owner write Settrade_participant_inn_owner;
    //Дата передачи товара
    property transfer_date:Tdate_type read Ftransfer_date write Settransfer_date;
    //Номер первичного документа
    property move_document_number:Tstring255_type read Fmove_document_number write Setmove_document_number;
    //Дата первичного документа
    property move_document_date:Tdate_type read Fmove_document_date write Setmove_document_date;
    //Вид оборота товаров
    property turnover_type:Tturnover_enum_type read Fturnover_type write Setturnover_type;
    //Причина вывода из оборота
    property withdrawal_type:Twithdrawal_shipment_type read Fwithdrawal_type write Setwithdrawal_type;
    //Дата вывода из оборота
    property withdrawal_date:Tdate_type read Fwithdrawal_date write Setwithdrawal_date;
    //Идентификатор гос.контракта
    property st_contract_id:Tstring255_type read Fst_contract_id write Setst_contract_id;
    //Отгрузка не участнику
    property to_not_participant:Boolean read Fto_not_participant write Setto_not_participant;
    //Параметры товаров
    property products_list:Tshipment_products_list read Fproducts_list;
    property action_id:Longint read Faction_id write Setaction_id;
    property version:Double read Fversion write Setversion;
  end;

  {  Tshipment_element  }
  Tshipment_element = class(Tshipment)
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

  {  Tshipment_products_list  }
  Tshipment_products_list = class(TXmlSerializationObject)
  private
    Fproduct:Tshipment_products_list_productList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Параметры товара
    property product:Tshipment_products_list_productList read Fproduct;
  end;

  {  Tshipment_products_list_product  }
  Tshipment_products_list_product = class(TXmlSerializationObject)
  private
    Fkit:Tkit_type;
    Fkitu:Tkitu_type;
    Fcost:Tprice_type;
    Fvat_value:Tprice_type;
    procedure Setkit( AValue:Tkit_type);
    procedure Setkitu( AValue:Tkitu_type);
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
    //КИТУ
    property kitu:Tkitu_type read Fkitu write Setkitu;
    //Цена за единицу
    property cost:Tprice_type read Fcost write Setcost;
    //Сумма НДС
    property vat_value:Tprice_type read Fvat_value write Setvat_value;
  end;

implementation

  {  Tshipment  }
procedure Tshipment.Settrade_participant_inn_sender(AValue: Tinn_type);
begin
  CheckStrMinSize('trade_participant_inn_sender', AValue);
  CheckStrMaxSize('trade_participant_inn_sender', AValue);
  Ftrade_participant_inn_sender:=AValue;
  ModifiedProperty('trade_participant_inn_sender');
end;

procedure Tshipment.Settrade_participant_inn_receiver(AValue: Tinn_type);
begin
  CheckStrMinSize('trade_participant_inn_receiver', AValue);
  CheckStrMaxSize('trade_participant_inn_receiver', AValue);
  Ftrade_participant_inn_receiver:=AValue;
  ModifiedProperty('trade_participant_inn_receiver');
end;

procedure Tshipment.Settrade_participant_inn_owner(AValue: Tinn_type);
begin
  CheckStrMinSize('trade_participant_inn_owner', AValue);
  CheckStrMaxSize('trade_participant_inn_owner', AValue);
  Ftrade_participant_inn_owner:=AValue;
  ModifiedProperty('trade_participant_inn_owner');
end;

procedure Tshipment.Settransfer_date(AValue: Tdate_type);
begin
  CheckStrMinSize('transfer_date', AValue);
  CheckStrMaxSize('transfer_date', AValue);
  Ftransfer_date:=AValue;
  ModifiedProperty('transfer_date');
end;

procedure Tshipment.Setmove_document_number(AValue: Tstring255_type);
begin
  CheckStrMinSize('move_document_number', AValue);
  CheckStrMaxSize('move_document_number', AValue);
  Fmove_document_number:=AValue;
  ModifiedProperty('move_document_number');
end;

procedure Tshipment.Setmove_document_date(AValue: Tdate_type);
begin
  CheckStrMinSize('move_document_date', AValue);
  CheckStrMaxSize('move_document_date', AValue);
  Fmove_document_date:=AValue;
  ModifiedProperty('move_document_date');
end;

procedure Tshipment.Setturnover_type(AValue: Tturnover_enum_type);
begin
  CheckLockupValue('turnover_type', AValue);
  Fturnover_type:=AValue;
  ModifiedProperty('turnover_type');
end;

procedure Tshipment.Setwithdrawal_type(AValue: Twithdrawal_shipment_type);
begin
  CheckLockupValue('withdrawal_type', AValue);
  Fwithdrawal_type:=AValue;
  ModifiedProperty('withdrawal_type');
end;

procedure Tshipment.Setwithdrawal_date(AValue: Tdate_type);
begin
  CheckStrMinSize('withdrawal_date', AValue);
  CheckStrMaxSize('withdrawal_date', AValue);
  Fwithdrawal_date:=AValue;
  ModifiedProperty('withdrawal_date');
end;

procedure Tshipment.Setst_contract_id(AValue: Tstring255_type);
begin
  CheckStrMinSize('st_contract_id', AValue);
  CheckStrMaxSize('st_contract_id', AValue);
  Fst_contract_id:=AValue;
  ModifiedProperty('st_contract_id');
end;

procedure Tshipment.Setto_not_participant(AValue: Boolean);
begin
  Fto_not_participant:=AValue;
  ModifiedProperty('to_not_participant');
end;

procedure Tshipment.Setaction_id(AValue: Longint);
begin
  CheckFixedValue('action_id', AValue);
  Faction_id:=AValue;
  ModifiedProperty('action_id');
end;

procedure Tshipment.Setversion(AValue: Double);
begin
  CheckFixedValue('version', AValue);
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure Tshipment.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('trade_participant_inn_sender', 'trade_participant_inn_sender', [xsaSimpleObject], '', 9, 12);
  P:=RegisterProperty('trade_participant_inn_receiver', 'trade_participant_inn_receiver', [xsaSimpleObject], '', 9, 12);
  P:=RegisterProperty('trade_participant_inn_owner', 'trade_participant_inn_owner', [xsaSimpleObject], '', 9, 12);
  P:=RegisterProperty('transfer_date', 'transfer_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('move_document_number', 'move_document_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('move_document_date', 'move_document_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('turnover_type', 'turnover_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('SELLING');
    P.ValidList.Add('COMMISSION');
    P.ValidList.Add('AGENT');
    P.ValidList.Add('COMMISSIONAIRE_SALE');
    P.ValidList.Add('CONTRACT');
  P:=RegisterProperty('withdrawal_type', 'withdrawal_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('DONATION');
    P.ValidList.Add('STATE_ENTERPRISE');
    P.ValidList.Add('NO_RETAIL_USE');
  P:=RegisterProperty('withdrawal_date', 'withdrawal_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('st_contract_id', 'st_contract_id', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('to_not_participant', 'to_not_participant', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('products_list', 'products_list', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='10';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='3';
end;

procedure Tshipment.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproducts_list:=Tshipment_products_list.Create;
end;

destructor Tshipment.Destroy;
begin
  Fproducts_list.Free;
  inherited Destroy;
end;

constructor Tshipment.Create;
begin
  inherited Create;
  action_id:=10;
  version:=3;
end;

  {  Tshipment_element  }
procedure Tshipment_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Tshipment_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tshipment_element.Destroy;
begin
  inherited Destroy;
end;

function Tshipment_element.RootNodeName:string;
begin
  Result:='shipment';
end;

constructor Tshipment_element.Create;
begin
  inherited Create;
end;

  {  Tshipment_products_list  }
procedure Tshipment_products_list.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('product', 'product', [], '', -1, -1);
end;

procedure Tshipment_products_list.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fproduct:=Tshipment_products_list_productList.Create;
end;

destructor Tshipment_products_list.Destroy;
begin
  Fproduct.Free;
  inherited Destroy;
end;

constructor Tshipment_products_list.Create;
begin
  inherited Create;
end;

  {  Tshipment_products_list_product  }
procedure Tshipment_products_list_product.Setkit(AValue: Tkit_type);
begin
  CheckStrMinSize('kit', AValue);
  CheckStrMaxSize('kit', AValue);
  Fkit:=AValue;
  ModifiedProperty('kit');
end;

procedure Tshipment_products_list_product.Setkitu(AValue: Tkitu_type);
begin
  CheckStrMinSize('kitu', AValue);
  CheckStrMaxSize('kitu', AValue);
  Fkitu:=AValue;
  ModifiedProperty('kitu');
end;

procedure Tshipment_products_list_product.Setcost(AValue: Tprice_type);
begin
  CheckMinInclusiveValue('cost', AValue);
  Fcost:=AValue;
  ModifiedProperty('cost');
end;

procedure Tshipment_products_list_product.Setvat_value(AValue: Tprice_type);
begin
  CheckMinInclusiveValue('vat_value', AValue);
  Fvat_value:=AValue;
  ModifiedProperty('vat_value');
end;

procedure Tshipment_products_list_product.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('kit', 'kit', [xsaSimpleObject], '', 25, 45);
  P:=RegisterProperty('kitu', 'kitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('cost', 'cost', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('vat_value', 'vat_value', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
end;

procedure Tshipment_products_list_product.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tshipment_products_list_product.Destroy;
begin
  inherited Destroy;
end;

constructor Tshipment_products_list_product.Create;
begin
  inherited Create;
end;

end.
