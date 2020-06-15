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

unit shipment_signs;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, LP_base_types;

type
  Taoguid = String;
  Thouseguid = String;
  Tflat = String;
type

  {  Forward declarations  }
  Tfias_address_type = class;
  Tshipment_signs = class;
  Tshipment_signs_element = class;
  Tshipment_signs_order_details = class;
  Tshipment_signs_order_details_signs = class;
  Tshipment_signs_order_details_signs_sign_uit = class;
  Tshipment_signs_order_details_signs_sign_uitu = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  Tshipment_signsList = specialize GXMLSerializationObjectList<Tshipment_signs>;
  Tshipment_signs_elementList = specialize GXMLSerializationObjectList<Tshipment_signs_element>;
  Tshipment_signs_order_detailsList = specialize GXMLSerializationObjectList<Tshipment_signs_order_details>;
  Tshipment_signs_order_details_signsList = specialize GXMLSerializationObjectList<Tshipment_signs_order_details_signs>;
  Tshipment_signs_order_details_signs_sign_uitList = specialize GXMLSerializationObjectList<Tshipment_signs_order_details_signs_sign_uit>;
  Tshipment_signs_order_details_signs_sign_uituList = specialize GXMLSerializationObjectList<Tshipment_signs_order_details_signs_sign_uitu>;

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

  {  Tshipment_signs  }
  //Отгрузка товара
  Tshipment_signs = class(TXmlSerializationObject)
  private
    Fsubject_inn:TTRADE_PARTICIPANT_INN_type;
    Frecipient_inn:TTRADE_PARTICIPANT_INN_type;
    Fowner_inn:TTRADE_PARTICIPANT_INN_type;
    Ftransfer_date:Tdate_type;
    Fmove_doc_num:Tdocument_number_200_type;
    Fmove_doc_date:Tdate_type;
    Fturnover_type:Tturnover_type_enum;
    Forder_details:Tshipment_signs_order_details;
    Faction_id:Longint;
    Fversion:Double;
    procedure Setsubject_inn( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure Setrecipient_inn( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure Setowner_inn( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure Settransfer_date( AValue:Tdate_type);
    procedure Setmove_doc_num( AValue:Tdocument_number_200_type);
    procedure Setmove_doc_date( AValue:Tdate_type);
    procedure Setturnover_type( AValue:Tturnover_type_enum);
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
    property subject_inn:TTRADE_PARTICIPANT_INN_type read Fsubject_inn write Setsubject_inn;
    //ИНН получателя
    property recipient_inn:TTRADE_PARTICIPANT_INN_type read Frecipient_inn write Setrecipient_inn;
    //ИНН собственника товара
    property owner_inn:TTRADE_PARTICIPANT_INN_type read Fowner_inn write Setowner_inn;
    //Дата передачи товара
    property transfer_date:Tdate_type read Ftransfer_date write Settransfer_date;
    //Реквизиты первичных документов: номер документа
    property move_doc_num:Tdocument_number_200_type read Fmove_doc_num write Setmove_doc_num;
    //Реквизиты первичных документов: дата документа
    property move_doc_date:Tdate_type read Fmove_doc_date write Setmove_doc_date;
    //Вид оборота товара
    property turnover_type:Tturnover_type_enum read Fturnover_type write Setturnover_type;
    //Список отгружаемой продукции
    property order_details:Tshipment_signs_order_details read Forder_details;
    property action_id:Longint read Faction_id write Setaction_id;
    property version:Double read Fversion write Setversion;
  end;

  {  Tshipment_signs_element  }
  Tshipment_signs_element = class(Tshipment_signs)
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

  {  Tshipment_signs_order_details  }
  Tshipment_signs_order_details = class(TXmlSerializationObject)
  private
    Fsigns:Tshipment_signs_order_details_signs;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property signs:Tshipment_signs_order_details_signs read Fsigns;
  end;

  {  Tshipment_signs_order_details_signs  }
  Tshipment_signs_order_details_signs = class(TXmlSerializationObject)
  private
    Fsign_uit:Tshipment_signs_order_details_signs_sign_uitList;
    Fsign_uitu:Tshipment_signs_order_details_signs_sign_uituList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property sign_uit:Tshipment_signs_order_details_signs_sign_uitList read Fsign_uit;
    property sign_uitu:Tshipment_signs_order_details_signs_sign_uituList read Fsign_uitu;
  end;

  {  Tshipment_signs_order_details_signs_sign_uit  }
  Tshipment_signs_order_details_signs_sign_uit = class(TXmlSerializationObject)
  private
    Fuit:Tgs1_uit_type;
    Fcost:Tprice_type;
    Fvat_value:Tprice_type;
    procedure Setuit( AValue:Tgs1_uit_type);
    procedure Setcost( AValue:Tprice_type);
    procedure Setvat_value( AValue:Tprice_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Уникальный идентификатор товара
    property uit:Tgs1_uit_type read Fuit write Setuit;
    //Стоимость продукции
    property cost:Tprice_type read Fcost write Setcost;
    //НДС продукции (если сделка облагается НДС)
    property vat_value:Tprice_type read Fvat_value write Setvat_value;
  end;

  {  Tshipment_signs_order_details_signs_sign_uitu  }
  Tshipment_signs_order_details_signs_sign_uitu = class(TXmlSerializationObject)
  private
    Fuitu:Tgs1_uitu_type;
    Fcost:Tprice_type;
    Fvat_value:Tprice_type;
    procedure Setuitu( AValue:Tgs1_uitu_type);
    procedure Setcost( AValue:Tprice_type);
    procedure Setvat_value( AValue:Tprice_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Уникальный идентификатор товара
    property uitu:Tgs1_uitu_type read Fuitu write Setuitu;
    //Сумма стоимоси всей продукции в транспортной упаковке
    property cost:Tprice_type read Fcost write Setcost;
    //Сумма налога (НДС)
    property vat_value:Tprice_type read Fvat_value write Setvat_value;
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

  {  Tshipment_signs  }
procedure Tshipment_signs.Setsubject_inn(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  CheckStrMinSize('subject_inn', AValue);
  CheckStrMaxSize('subject_inn', AValue);
  Fsubject_inn:=AValue;
  ModifiedProperty('subject_inn');
end;

procedure Tshipment_signs.Setrecipient_inn(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  CheckStrMinSize('recipient_inn', AValue);
  CheckStrMaxSize('recipient_inn', AValue);
  Frecipient_inn:=AValue;
  ModifiedProperty('recipient_inn');
end;

procedure Tshipment_signs.Setowner_inn(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  CheckStrMinSize('owner_inn', AValue);
  CheckStrMaxSize('owner_inn', AValue);
  Fowner_inn:=AValue;
  ModifiedProperty('owner_inn');
end;

procedure Tshipment_signs.Settransfer_date(AValue: Tdate_type);
begin
  CheckStrMinSize('transfer_date', AValue);
  CheckStrMaxSize('transfer_date', AValue);
  Ftransfer_date:=AValue;
  ModifiedProperty('transfer_date');
end;

procedure Tshipment_signs.Setmove_doc_num(AValue: Tdocument_number_200_type);
begin
  CheckStrMinSize('move_doc_num', AValue);
  CheckStrMaxSize('move_doc_num', AValue);
  Fmove_doc_num:=AValue;
  ModifiedProperty('move_doc_num');
end;

procedure Tshipment_signs.Setmove_doc_date(AValue: Tdate_type);
begin
  CheckStrMinSize('move_doc_date', AValue);
  CheckStrMaxSize('move_doc_date', AValue);
  Fmove_doc_date:=AValue;
  ModifiedProperty('move_doc_date');
end;

procedure Tshipment_signs.Setturnover_type(AValue: Tturnover_type_enum);
begin
  CheckLockupValue('turnover_type', AValue);
  Fturnover_type:=AValue;
  ModifiedProperty('turnover_type');
end;

procedure Tshipment_signs.Setaction_id(AValue: Longint);
begin
  CheckFixedValue('action_id', AValue);
  Faction_id:=AValue;
  ModifiedProperty('action_id');
end;

procedure Tshipment_signs.Setversion(AValue: Double);
begin
  CheckFixedValue('version', AValue);
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure Tshipment_signs.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('subject_inn', 'subject_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('recipient_inn', 'recipient_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('owner_inn', 'owner_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('transfer_date', 'transfer_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('move_doc_num', 'move_doc_num', [xsaSimpleObject], '', 1, 200);
  P:=RegisterProperty('move_doc_date', 'move_doc_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('turnover_type', 'turnover_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
    P.ValidList.Add('3');
    P.ValidList.Add('4');
  P:=RegisterProperty('order_details', 'order_details', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='10';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='1.02';
end;

procedure Tshipment_signs.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Forder_details:=Tshipment_signs_order_details.Create;
end;

destructor Tshipment_signs.Destroy;
begin
  Forder_details.Free;
  inherited Destroy;
end;

constructor Tshipment_signs.Create;
begin
  inherited Create;
  action_id:=10;
  version:=1.02;
end;

  {  Tshipment_signs_element  }
procedure Tshipment_signs_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Tshipment_signs_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tshipment_signs_element.Destroy;
begin
  inherited Destroy;
end;

function Tshipment_signs_element.RootNodeName:string;
begin
  Result:='shipment_signs';
end;

constructor Tshipment_signs_element.Create;
begin
  inherited Create;
end;

  {  Tshipment_signs_order_details  }
procedure Tshipment_signs_order_details.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('signs', 'signs', [], '', -1, -1);
end;

procedure Tshipment_signs_order_details.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fsigns:=Tshipment_signs_order_details_signs.Create;
end;

destructor Tshipment_signs_order_details.Destroy;
begin
  Fsigns.Free;
  inherited Destroy;
end;

constructor Tshipment_signs_order_details.Create;
begin
  inherited Create;
end;

  {  Tshipment_signs_order_details_signs  }
procedure Tshipment_signs_order_details_signs.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('sign_uit', 'sign_uit', [], '', -1, -1);
  P:=RegisterProperty('sign_uitu', 'sign_uitu', [], '', -1, -1);
end;

procedure Tshipment_signs_order_details_signs.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fsign_uit:=Tshipment_signs_order_details_signs_sign_uitList.Create;
  Fsign_uitu:=Tshipment_signs_order_details_signs_sign_uituList.Create;
end;

destructor Tshipment_signs_order_details_signs.Destroy;
begin
  Fsign_uit.Free;
  Fsign_uitu.Free;
  inherited Destroy;
end;

constructor Tshipment_signs_order_details_signs.Create;
begin
  inherited Create;
end;

  {  Tshipment_signs_order_details_signs_sign_uit  }
procedure Tshipment_signs_order_details_signs_sign_uit.Setuit(AValue: Tgs1_uit_type);
begin
  CheckStrMinSize('uit', AValue);
  CheckStrMaxSize('uit', AValue);
  Fuit:=AValue;
  ModifiedProperty('uit');
end;

procedure Tshipment_signs_order_details_signs_sign_uit.Setcost(AValue: Tprice_type);
begin
  CheckMinInclusiveValue('cost', AValue);
  Fcost:=AValue;
  ModifiedProperty('cost');
end;

procedure Tshipment_signs_order_details_signs_sign_uit.Setvat_value(AValue: Tprice_type);
begin
  CheckMinInclusiveValue('vat_value', AValue);
  Fvat_value:=AValue;
  ModifiedProperty('vat_value');
end;

procedure Tshipment_signs_order_details_signs_sign_uit.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('uit', 'uit', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('cost', 'cost', [], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('vat_value', 'vat_value', [], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
end;

procedure Tshipment_signs_order_details_signs_sign_uit.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tshipment_signs_order_details_signs_sign_uit.Destroy;
begin
  inherited Destroy;
end;

constructor Tshipment_signs_order_details_signs_sign_uit.Create;
begin
  inherited Create;
end;

  {  Tshipment_signs_order_details_signs_sign_uitu  }
procedure Tshipment_signs_order_details_signs_sign_uitu.Setuitu(AValue: Tgs1_uitu_type);
begin
  CheckStrMinSize('uitu', AValue);
  CheckStrMaxSize('uitu', AValue);
  Fuitu:=AValue;
  ModifiedProperty('uitu');
end;

procedure Tshipment_signs_order_details_signs_sign_uitu.Setcost(AValue: Tprice_type);
begin
  CheckMinInclusiveValue('cost', AValue);
  Fcost:=AValue;
  ModifiedProperty('cost');
end;

procedure Tshipment_signs_order_details_signs_sign_uitu.Setvat_value(AValue: Tprice_type);
begin
  CheckMinInclusiveValue('vat_value', AValue);
  Fvat_value:=AValue;
  ModifiedProperty('vat_value');
end;

procedure Tshipment_signs_order_details_signs_sign_uitu.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('uitu', 'uitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('cost', 'cost', [], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('vat_value', 'vat_value', [], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
end;

procedure Tshipment_signs_order_details_signs_sign_uitu.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tshipment_signs_order_details_signs_sign_uitu.Destroy;
begin
  inherited Destroy;
end;

constructor Tshipment_signs_order_details_signs_sign_uitu.Create;
begin
  inherited Create;
end;

end.