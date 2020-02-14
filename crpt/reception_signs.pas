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

unit reception_signs;

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
  Treception_signs = class;
  Treception_signs_element = class;
  Treception_signs_order_details = class;
  Treception_signs_order_details_signs = class;
  Treception_signs_order_details_signs_sign_uit = class;
  Treception_signs_order_details_signs_sign_uitu = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  Treception_signsList = specialize GXMLSerializationObjectList<Treception_signs>;
  Treception_signs_elementList = specialize GXMLSerializationObjectList<Treception_signs_element>;
  Treception_signs_order_detailsList = specialize GXMLSerializationObjectList<Treception_signs_order_details>;
  Treception_signs_order_details_signsList = specialize GXMLSerializationObjectList<Treception_signs_order_details_signs>;
  Treception_signs_order_details_signs_sign_uitList = specialize GXMLSerializationObjectList<Treception_signs_order_details_signs_sign_uit>;
  Treception_signs_order_details_signs_sign_uituList = specialize GXMLSerializationObjectList<Treception_signs_order_details_signs_sign_uitu>;

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

  {  Treception_signs  }
  //Прием товара
  Treception_signs = class(TXmlSerializationObject)
  private
    Fsession_ui:Tguid_type;
    Fsubject_inn:TTRADE_PARTICIPANT_INN_type;
    Frecipient_inn:TTRADE_PARTICIPANT_INN_type;
    Fowner_inn:TTRADE_PARTICIPANT_INN_type;
    Fsender_date:Tdatetimeoffset;
    Frecipient_date:Tdatetimeoffset;
    Ftransfer_date:Tdate_type;
    Fmove_doc_num:Tdocument_number_200_type;
    Fmove_doc_date:Tdate_type;
    Fturnover_type:Tturnover_type_enum;
    Forder_details:Treception_signs_order_details;
    Faction_id:Longint;
    Fversion:Double;
    procedure Setsession_ui( AValue:Tguid_type);
    procedure Setsubject_inn( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure Setrecipient_inn( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure Setowner_inn( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure Setsender_date( AValue:Tdatetimeoffset);
    procedure Setrecipient_date( AValue:Tdatetimeoffset);
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
    //ID заявки на отгрузку
    property session_ui:Tguid_type read Fsession_ui write Setsession_ui;
    //ИНН отправителя
    property subject_inn:TTRADE_PARTICIPANT_INN_type read Fsubject_inn write Setsubject_inn;
    //ИНН получателя
    property recipient_inn:TTRADE_PARTICIPANT_INN_type read Frecipient_inn write Setrecipient_inn;
    //ИНН собственника товара
    property owner_inn:TTRADE_PARTICIPANT_INN_type read Fowner_inn write Setowner_inn;
    //Дата и время отгрузки
    property sender_date:Tdatetimeoffset read Fsender_date write Setsender_date;
    //Дата и время приемки
    property recipient_date:Tdatetimeoffset read Frecipient_date write Setrecipient_date;
    //Дата и время приемки
    property transfer_date:Tdate_type read Ftransfer_date write Settransfer_date;
    //Реквизиты первичных документов: номер документа
    property move_doc_num:Tdocument_number_200_type read Fmove_doc_num write Setmove_doc_num;
    //Реквизиты первичных документов: дата документа
    property move_doc_date:Tdate_type read Fmove_doc_date write Setmove_doc_date;
    //Вид оборота товара
    property turnover_type:Tturnover_type_enum read Fturnover_type write Setturnover_type;
    //Список принимаемой и не принимаемой продукции
    property order_details:Treception_signs_order_details read Forder_details;
    property action_id:Longint read Faction_id write Setaction_id;
    property version:Double read Fversion write Setversion;
  end;

  {  Treception_signs_element  }
  Treception_signs_element = class(Treception_signs)
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

  {  Treception_signs_order_details  }
  Treception_signs_order_details = class(TXmlSerializationObject)
  private
    Fsigns:Treception_signs_order_details_signs;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property signs:Treception_signs_order_details_signs read Fsigns;
  end;

  {  Treception_signs_order_details_signs  }
  Treception_signs_order_details_signs = class(TXmlSerializationObject)
  private
    Fsign_uit:Treception_signs_order_details_signs_sign_uitList;
    Fsign_uitu:Treception_signs_order_details_signs_sign_uituList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    property sign_uit:Treception_signs_order_details_signs_sign_uitList read Fsign_uit;
    property sign_uitu:Treception_signs_order_details_signs_sign_uituList read Fsign_uitu;
  end;

  {  Treception_signs_order_details_signs_sign_uit  }
  Treception_signs_order_details_signs_sign_uit = class(TXmlSerializationObject)
  private
    Fuit:Tgs1_uit_type;
    Faccept_type:Boolean;
    Fcost:Tprice_type;
    Fvat_value:Tprice_type;
    procedure Setuit( AValue:Tgs1_uit_type);
    procedure Setaccept_type( AValue:Boolean);
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
    //Признак принятия товара
    property accept_type:Boolean read Faccept_type write Setaccept_type;
    //Стоимость продукции
    property cost:Tprice_type read Fcost write Setcost;
    //НДС продукции (если сделка облагается НДС)
    property vat_value:Tprice_type read Fvat_value write Setvat_value;
  end;

  {  Treception_signs_order_details_signs_sign_uitu  }
  Treception_signs_order_details_signs_sign_uitu = class(TXmlSerializationObject)
  private
    Fuitu:Tgs1_uitu_type;
    Faccept_type:Boolean;
    Fcost:Tprice_type;
    Fvat_value:Tprice_type;
    procedure Setuitu( AValue:Tgs1_uitu_type);
    procedure Setaccept_type( AValue:Boolean);
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
    //Признак принятия товара
    property accept_type:Boolean read Faccept_type write Setaccept_type;
    //Сумма стоимоси всей продукции в транспортной упаковке
    property cost:Tprice_type read Fcost write Setcost;
    //НДС продукции (если сделка облагается НДС)
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

  {  Treception_signs  }
procedure Treception_signs.Setsession_ui(AValue: Tguid_type);
begin
  Fsession_ui:=AValue;
  CheckStrMinSize('session_ui', AValue);
  CheckStrMaxSize('session_ui', AValue);
  ModifiedProperty('session_ui');
end;

procedure Treception_signs.Setsubject_inn(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  Fsubject_inn:=AValue;
  CheckStrMinSize('subject_inn', AValue);
  CheckStrMaxSize('subject_inn', AValue);
  ModifiedProperty('subject_inn');
end;

procedure Treception_signs.Setrecipient_inn(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  Frecipient_inn:=AValue;
  CheckStrMinSize('recipient_inn', AValue);
  CheckStrMaxSize('recipient_inn', AValue);
  ModifiedProperty('recipient_inn');
end;

procedure Treception_signs.Setowner_inn(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  Fowner_inn:=AValue;
  CheckStrMinSize('owner_inn', AValue);
  CheckStrMaxSize('owner_inn', AValue);
  ModifiedProperty('owner_inn');
end;

procedure Treception_signs.Setsender_date(AValue: Tdatetimeoffset);
begin
  Fsender_date:=AValue;
  ModifiedProperty('sender_date');
end;

procedure Treception_signs.Setrecipient_date(AValue: Tdatetimeoffset);
begin
  Frecipient_date:=AValue;
  ModifiedProperty('recipient_date');
end;

procedure Treception_signs.Settransfer_date(AValue: Tdate_type);
begin
  Ftransfer_date:=AValue;
  CheckStrMinSize('transfer_date', AValue);
  CheckStrMaxSize('transfer_date', AValue);
  ModifiedProperty('transfer_date');
end;

procedure Treception_signs.Setmove_doc_num(AValue: Tdocument_number_200_type);
begin
  Fmove_doc_num:=AValue;
  CheckStrMinSize('move_doc_num', AValue);
  CheckStrMaxSize('move_doc_num', AValue);
  ModifiedProperty('move_doc_num');
end;

procedure Treception_signs.Setmove_doc_date(AValue: Tdate_type);
begin
  Fmove_doc_date:=AValue;
  CheckStrMinSize('move_doc_date', AValue);
  CheckStrMaxSize('move_doc_date', AValue);
  ModifiedProperty('move_doc_date');
end;

procedure Treception_signs.Setturnover_type(AValue: Tturnover_type_enum);
begin
  Fturnover_type:=AValue;
  CheckLockupValue('turnover_type', AValue);
  ModifiedProperty('turnover_type');
end;

procedure Treception_signs.Setaction_id(AValue: Longint);
begin
  Faction_id:=AValue;
  CheckFixedValue('action_id', AValue);
  ModifiedProperty('action_id');
end;

procedure Treception_signs.Setversion(AValue: Double);
begin
  Fversion:=AValue;
  CheckFixedValue('version', AValue);
  ModifiedProperty('version');
end;

procedure Treception_signs.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('session_ui', 'session_ui', [xsaSimpleObject], '', 36, 36);
  P:=RegisterProperty('subject_inn', 'subject_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('recipient_inn', 'recipient_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('owner_inn', 'owner_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('sender_date', 'sender_date', [xsaSimpleObject], '', -1, -1);
  P:=RegisterProperty('recipient_date', 'recipient_date', [xsaSimpleObject], '', -1, -1);
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
    P.DefaultValue:='11';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='1.02';
end;

procedure Treception_signs.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Forder_details:=Treception_signs_order_details.Create;
end;

destructor Treception_signs.Destroy;
begin
  Forder_details.Free;
  inherited Destroy;
end;

constructor Treception_signs.Create;
begin
  inherited Create;
  action_id:=11;
  version:=1.02;
end;

  {  Treception_signs_element  }
procedure Treception_signs_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Treception_signs_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Treception_signs_element.Destroy;
begin
  inherited Destroy;
end;

function Treception_signs_element.RootNodeName:string;
begin
  Result:='reception_signs';
end;

constructor Treception_signs_element.Create;
begin
  inherited Create;
end;

  {  Treception_signs_order_details  }
procedure Treception_signs_order_details.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('signs', 'signs', [], '', -1, -1);
end;

procedure Treception_signs_order_details.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fsigns:=Treception_signs_order_details_signs.Create;
end;

destructor Treception_signs_order_details.Destroy;
begin
  Fsigns.Free;
  inherited Destroy;
end;

constructor Treception_signs_order_details.Create;
begin
  inherited Create;
end;

  {  Treception_signs_order_details_signs  }
procedure Treception_signs_order_details_signs.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('sign_uit', 'sign_uit', [], '', -1, -1);
  P:=RegisterProperty('sign_uitu', 'sign_uitu', [], '', -1, -1);
end;

procedure Treception_signs_order_details_signs.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fsign_uit:=Treception_signs_order_details_signs_sign_uitList.Create;
  Fsign_uitu:=Treception_signs_order_details_signs_sign_uituList.Create;
end;

destructor Treception_signs_order_details_signs.Destroy;
begin
  Fsign_uit.Free;
  Fsign_uitu.Free;
  inherited Destroy;
end;

constructor Treception_signs_order_details_signs.Create;
begin
  inherited Create;
end;

  {  Treception_signs_order_details_signs_sign_uit  }
procedure Treception_signs_order_details_signs_sign_uit.Setuit(AValue: Tgs1_uit_type);
begin
  Fuit:=AValue;
  CheckStrMinSize('uit', AValue);
  CheckStrMaxSize('uit', AValue);
  ModifiedProperty('uit');
end;

procedure Treception_signs_order_details_signs_sign_uit.Setaccept_type(AValue: Boolean);
begin
  Faccept_type:=AValue;
  ModifiedProperty('accept_type');
end;

procedure Treception_signs_order_details_signs_sign_uit.Setcost(AValue: Tprice_type);
begin
  Fcost:=AValue;
  CheckMinInclusiveValue('cost', AValue);
  ModifiedProperty('cost');
end;

procedure Treception_signs_order_details_signs_sign_uit.Setvat_value(AValue: Tprice_type);
begin
  Fvat_value:=AValue;
  CheckMinInclusiveValue('vat_value', AValue);
  ModifiedProperty('vat_value');
end;

procedure Treception_signs_order_details_signs_sign_uit.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('uit', 'uit', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('accept_type', 'accept_type', [xsaRequared], '', -1, -1);
  P:=RegisterProperty('cost', 'cost', [], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('vat_value', 'vat_value', [], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
end;

procedure Treception_signs_order_details_signs_sign_uit.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Treception_signs_order_details_signs_sign_uit.Destroy;
begin
  inherited Destroy;
end;

constructor Treception_signs_order_details_signs_sign_uit.Create;
begin
  inherited Create;
end;

  {  Treception_signs_order_details_signs_sign_uitu  }
procedure Treception_signs_order_details_signs_sign_uitu.Setuitu(AValue: Tgs1_uitu_type);
begin
  Fuitu:=AValue;
  CheckStrMinSize('uitu', AValue);
  CheckStrMaxSize('uitu', AValue);
  ModifiedProperty('uitu');
end;

procedure Treception_signs_order_details_signs_sign_uitu.Setaccept_type(AValue: Boolean);
begin
  Faccept_type:=AValue;
  ModifiedProperty('accept_type');
end;

procedure Treception_signs_order_details_signs_sign_uitu.Setcost(AValue: Tprice_type);
begin
  Fcost:=AValue;
  CheckMinInclusiveValue('cost', AValue);
  ModifiedProperty('cost');
end;

procedure Treception_signs_order_details_signs_sign_uitu.Setvat_value(AValue: Tprice_type);
begin
  Fvat_value:=AValue;
  CheckMinInclusiveValue('vat_value', AValue);
  ModifiedProperty('vat_value');
end;

procedure Treception_signs_order_details_signs_sign_uitu.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('uitu', 'uitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('accept_type', 'accept_type', [xsaRequared], '', -1, -1);
  P:=RegisterProperty('cost', 'cost', [], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('vat_value', 'vat_value', [], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
end;

procedure Treception_signs_order_details_signs_sign_uitu.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Treception_signs_order_details_signs_sign_uitu.Destroy;
begin
  inherited Destroy;
end;

constructor Treception_signs_order_details_signs_sign_uitu.Create;
begin
  inherited Create;
end;

end.