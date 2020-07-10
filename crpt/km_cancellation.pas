{ GS1 interface library for FPC and Lazarus

  Copyright (C) 2020 Lagunov Aleksey alexs75@yandex.ru

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

unit km_cancellation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xmlobject, LP_base_types, AbstractSerializationObjects;

type
  Taoguid = String;
  Thouseguid = String;
  Tflat = String;
  Tcancellation_reason = String;
  Tcancellation_reason1 = String;
type

  {  Forward declarations  }
  Tfias_address_type = class;
  Tkm_cancellation = class;
  Tkm_cancellation_element = class;
  Tkm_cancellation_km_list = class;
  Tkm_cancellation_km_list_km = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  Tkm_cancellationList = specialize GXMLSerializationObjectList<Tkm_cancellation>;
  Tkm_cancellation_elementList = specialize GXMLSerializationObjectList<Tkm_cancellation_element>;
  Tkm_cancellation_km_listList = specialize GXMLSerializationObjectList<Tkm_cancellation_km_list>;
  Tkm_cancellation_km_list_kmList = specialize GXMLSerializationObjectList<Tkm_cancellation_km_list_km>;

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

  {  Tkm_cancellation  }
  //Списание кодов маркировки
  Tkm_cancellation = class(TXmlSerializationObject)
  private
    Ftrade_participant_inn:TTRADE_PARTICIPANT_INN_type;
    Fcancellation_reason:Tcancellation_reason;
    Fcancellation_doc_date:Tdate_type;
    Fcancellation_doc_number:Tstring255_type;
    Fkm_list:Tkm_cancellation_km_list;
    Faction_id:Longint;
    Fversion:Longint;
    procedure Settrade_participant_inn( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure Setcancellation_reason( AValue:Tcancellation_reason);
    procedure Setcancellation_doc_date( AValue:Tdate_type);
    procedure Setcancellation_doc_number( AValue:Tstring255_type);
    procedure Setaction_id( AValue:Longint);
    procedure Setversion( AValue:Longint);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //ИНН участника, осуществляющего списание кодов маркировки
    property trade_participant_inn:TTRADE_PARTICIPANT_INN_type read Ftrade_participant_inn write Settrade_participant_inn;
    //Причина списания
    //Испорчен - KM_SPOILED
    //Утерян - KM_LOST
    //Уничтожен - KM_DESTROYED
    property cancellation_reason:Tcancellation_reason read Fcancellation_reason write Setcancellation_reason;
    //Дата документа, подтверждающего списание
    property cancellation_doc_date:Tdate_type read Fcancellation_doc_date write Setcancellation_doc_date;
    //Номер документа, подтверждающего списание
    property cancellation_doc_number:Tstring255_type read Fcancellation_doc_number write Setcancellation_doc_number;
    property km_list:Tkm_cancellation_km_list read Fkm_list;
    property action_id:Longint read Faction_id write Setaction_id;
    property version:Longint read Fversion write Setversion;
  end;

  {  Tkm_cancellation_element  }
  Tkm_cancellation_element = class(Tkm_cancellation)
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

  {  Tkm_cancellation_km_list  }
  Tkm_cancellation_km_list = class(TXmlSerializationObject)
  private
    Fkm:Tkm_cancellation_km_list_kmList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Параметры списка кодов маркировки
    property km:Tkm_cancellation_km_list_kmList read Fkm;
  end;

  {  Tkm_cancellation_km_list_km  }
  Tkm_cancellation_km_list_km = class(TXmlSerializationObject)
  private
    Fuit:Tgs1_uit_type;
    Fuitu:Tgs1_uitu_type;
    Fcancellation_reason:Tcancellation_reason1;
    procedure Setuit( AValue:Tgs1_uit_type);
    procedure Setuitu( AValue:Tgs1_uitu_type);
    procedure Setcancellation_reason( AValue:Tcancellation_reason1);
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
    //Причина списания
    //Испорчен
    //Утерян
    //Уничтожен
    property cancellation_reason:Tcancellation_reason1 read Fcancellation_reason write Setcancellation_reason;
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

  {  Tkm_cancellation  }
procedure Tkm_cancellation.Settrade_participant_inn(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  CheckStrMinSize('trade_participant_inn', AValue);
  CheckStrMaxSize('trade_participant_inn', AValue);
  Ftrade_participant_inn:=AValue;
  ModifiedProperty('trade_participant_inn');
end;

procedure Tkm_cancellation.Setcancellation_reason(AValue: Tcancellation_reason);
begin
  CheckLockupValue('cancellation_reason', AValue);
  Fcancellation_reason:=AValue;
  ModifiedProperty('cancellation_reason');
end;

procedure Tkm_cancellation.Setcancellation_doc_date(AValue: Tdate_type);
begin
  CheckStrMinSize('cancellation_doc_date', AValue);
  CheckStrMaxSize('cancellation_doc_date', AValue);
  Fcancellation_doc_date:=AValue;
  ModifiedProperty('cancellation_doc_date');
end;

procedure Tkm_cancellation.Setcancellation_doc_number(AValue: Tstring255_type);
begin
  CheckStrMinSize('cancellation_doc_number', AValue);
  CheckStrMaxSize('cancellation_doc_number', AValue);
  Fcancellation_doc_number:=AValue;
  ModifiedProperty('cancellation_doc_number');
end;

procedure Tkm_cancellation.Setaction_id(AValue: Longint);
begin
  CheckFixedValue('action_id', AValue);
  Faction_id:=AValue;
  ModifiedProperty('action_id');
end;

procedure Tkm_cancellation.Setversion(AValue: Longint);
begin
  CheckFixedValue('version', AValue);
  Fversion:=AValue;
  ModifiedProperty('version');
end;

procedure Tkm_cancellation.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('trade_participant_inn', 'trade_participant_inn', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('cancellation_reason', 'cancellation_reason', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('KM_SPOILED');
    P.ValidList.Add('KM_LOST');
    P.ValidList.Add('KM_DESTROYED');
  P:=RegisterProperty('cancellation_doc_date', 'cancellation_doc_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('cancellation_doc_number', 'cancellation_doc_number', [xsaSimpleObject], '', 1, 255);
  P:=RegisterProperty('km_list', 'km_list', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='14';
  P:=RegisterProperty('version', 'version', [xsaRequared], '', -1, -1);
    P.DefaultValue:='1';
end;

procedure Tkm_cancellation.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fkm_list:=Tkm_cancellation_km_list.Create;
end;

destructor Tkm_cancellation.Destroy;
begin
  Fkm_list.Free;
  inherited Destroy;
end;

constructor Tkm_cancellation.Create;
begin
  inherited Create;
  action_id:=14;
  version:=1;
end;

  {  Tkm_cancellation_element  }
procedure Tkm_cancellation_element.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
end;

procedure Tkm_cancellation_element.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tkm_cancellation_element.Destroy;
begin
  inherited Destroy;
end;

function Tkm_cancellation_element.RootNodeName:string;
begin
  Result:='km_cancellation';
end;

constructor Tkm_cancellation_element.Create;
begin
  inherited Create;
end;

  {  Tkm_cancellation_km_list  }
procedure Tkm_cancellation_km_list.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('km', 'km', [], '', -1, -1);
end;

procedure Tkm_cancellation_km_list.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fkm:=Tkm_cancellation_km_list_kmList.Create;
end;

destructor Tkm_cancellation_km_list.Destroy;
begin
  Fkm.Free;
  inherited Destroy;
end;

constructor Tkm_cancellation_km_list.Create;
begin
  inherited Create;
end;

  {  Tkm_cancellation_km_list_km  }
procedure Tkm_cancellation_km_list_km.Setuit(AValue: Tgs1_uit_type);
begin
  CheckStrMinSize('uit', AValue);
  CheckStrMaxSize('uit', AValue);
  Fuit:=AValue;
  ModifiedProperty('uit');
end;

procedure Tkm_cancellation_km_list_km.Setuitu(AValue: Tgs1_uitu_type);
begin
  CheckStrMinSize('uitu', AValue);
  CheckStrMaxSize('uitu', AValue);
  Fuitu:=AValue;
  ModifiedProperty('uitu');
end;

procedure Tkm_cancellation_km_list_km.Setcancellation_reason(AValue: Tcancellation_reason1);
begin
  CheckLockupValue('cancellation_reason', AValue);
  Fcancellation_reason:=AValue;
  ModifiedProperty('cancellation_reason');
end;

procedure Tkm_cancellation_km_list_km.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('uit', 'uit', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('uitu', 'uitu', [xsaSimpleObject], '', 18, 18);
  P:=RegisterProperty('cancellation_reason', 'cancellation_reason', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('KM_SPOILED');
    P.ValidList.Add('KM_LOST');
    P.ValidList.Add('KM_DESTROYED');
end;

procedure Tkm_cancellation_km_list_km.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tkm_cancellation_km_list_km.Destroy;
begin
  inherited Destroy;
end;

constructor Tkm_cancellation_km_list_km.Create;
begin
  inherited Create;
end;

end.
