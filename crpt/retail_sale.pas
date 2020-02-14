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

unit retail_sale;

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
  Tretail_sale = class;
  Tretail_sale_conclusion = class;
  Tretail_sale_conclusion_detail = class;

  {  Generic classes for collections  }
  Tfias_address_typeList = specialize GXMLSerializationObjectList<Tfias_address_type>;
  Tretail_saleList = specialize GXMLSerializationObjectList<Tretail_sale>;
  Tretail_sale_conclusionList = specialize GXMLSerializationObjectList<Tretail_sale_conclusion>;
  Tretail_sale_conclusion_detailList = specialize GXMLSerializationObjectList<Tretail_sale_conclusion_detail>;

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

  {  Tretail_sale  }
  //Вывод из оборота товара при розничной продаже
  Tretail_sale = class(TXmlSerializationObject)
  private
    FTRADE_PARTICIPANT_INN:TTRADE_PARTICIPANT_INN_type;
    Faction:Taction_type;
    Faction_date:Tdate_type;
    Fdoc_type:Tdoc_type_enum;
    Fdoc_name:Tstring200_type;
    Fdoc_number:Tstring200_type;
    Fdoc_date:Tdate_type;
    Fconclusion:Tretail_sale_conclusion;
    Faction_id:Longint;
    procedure SetTRADE_PARTICIPANT_INN( AValue:TTRADE_PARTICIPANT_INN_type);
    procedure Setaction( AValue:Taction_type);
    procedure Setaction_date( AValue:Tdate_type);
    procedure Setdoc_type( AValue:Tdoc_type_enum);
    procedure Setdoc_name( AValue:Tstring200_type);
    procedure Setdoc_number( AValue:Tstring200_type);
    procedure Setdoc_date( AValue:Tdate_type);
    procedure Setaction_id( AValue:Longint);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //ИНН участника, осуществляющего вывод из оборота
    property TRADE_PARTICIPANT_INN:TTRADE_PARTICIPANT_INN_type read FTRADE_PARTICIPANT_INN write SetTRADE_PARTICIPANT_INN;
    //Вид вывода из оборота:
    //1 - Розничная продажа
    //2 - Экспорт в страны ЕАЭС
    //3 - Экспорт за пределы стран ЕАЭС
    //4 - Кредитный договор
    //5 - Порча или утеря товара
    //6 - Безвозмездная передача
    //7 - Возврат физическому лицу (при невозможности реализовать товар по договору комиссии товар возвращается физическому лицу обратно)
    //8 - Банкротство или ликвидация ЮЛ/ИП
    //9 - Реализации конфискованных товаров
    //10 - Использование на предприятии
    //11 - Реализация по договору рассрочки
    //12 - В процессе реализации по договору рассрочки
    property action:Taction_type read Faction write Setaction;
    //Дата вывода товара из оборота
    property action_date:Tdate_type read Faction_date write Setaction_date;
    //Вид документа
    //1 - Фискальный документ
    //2 - Бланк строгой отчетности (БСО)
    //3 - Договор
    //4 - Акт уничтожения
    //5 - Товарная накладная
    //6 - Счет-фактура
    //7 - Прочее
    property doc_type:Tdoc_type_enum read Fdoc_type write Setdoc_type;
    //Наименование документа
    property doc_name:Tstring200_type read Fdoc_name write Setdoc_name;
    //Номер документа
    property doc_number:Tstring200_type read Fdoc_number write Setdoc_number;
    //Дата документа
    property doc_date:Tdate_type read Fdoc_date write Setdoc_date;
    property conclusion:Tretail_sale_conclusion read Fconclusion;
    property action_id:Longint read Faction_id write Setaction_id;
  end;

  {  Tretail_sale_conclusion  }
  Tretail_sale_conclusion = class(TXmlSerializationObject)
  private
    Fdetail:Tretail_sale_conclusion_detailList;
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Детализация вывода из оборота по каждой единице продукции
    property detail:Tretail_sale_conclusion_detailList read Fdetail;
  end;

  {  Tretail_sale_conclusion_detail  }
  Tretail_sale_conclusion_detail = class(TXmlSerializationObject)
  private
    Fsign_num:Tgs1_uit_type;
    Fcost:Tprice_type;
    Fvat_value:Tprice_type;
    procedure Setsign_num( AValue:Tgs1_uit_type);
    procedure Setcost( AValue:Tprice_type);
    procedure Setvat_value( AValue:Tprice_type);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    constructor Create;
    destructor Destroy; override;
  published
    //Номер КиЗ/УИТ
    property sign_num:Tgs1_uit_type read Fsign_num write Setsign_num;
    //Стоимость (в руб. с учетом НДС)
    property cost:Tprice_type read Fcost write Setcost;
    //Сумма НДС (если сделка облагается НДС)
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

  {  Tretail_sale  }
procedure Tretail_sale.SetTRADE_PARTICIPANT_INN(AValue: TTRADE_PARTICIPANT_INN_type);
begin
  FTRADE_PARTICIPANT_INN:=AValue;
  CheckStrMinSize('TRADE_PARTICIPANT_INN', AValue);
  CheckStrMaxSize('TRADE_PARTICIPANT_INN', AValue);
  ModifiedProperty('TRADE_PARTICIPANT_INN');
end;

procedure Tretail_sale.Setaction(AValue: Taction_type);
begin
  Faction:=AValue;
  CheckLockupValue('action', AValue);
  ModifiedProperty('action');
end;

procedure Tretail_sale.Setaction_date(AValue: Tdate_type);
begin
  Faction_date:=AValue;
  CheckStrMinSize('action_date', AValue);
  CheckStrMaxSize('action_date', AValue);
  ModifiedProperty('action_date');
end;

procedure Tretail_sale.Setdoc_type(AValue: Tdoc_type_enum);
begin
  Fdoc_type:=AValue;
  CheckLockupValue('doc_type', AValue);
  ModifiedProperty('doc_type');
end;

procedure Tretail_sale.Setdoc_name(AValue: Tstring200_type);
begin
  Fdoc_name:=AValue;
  CheckStrMinSize('doc_name', AValue);
  CheckStrMaxSize('doc_name', AValue);
  ModifiedProperty('doc_name');
end;

procedure Tretail_sale.Setdoc_number(AValue: Tstring200_type);
begin
  Fdoc_number:=AValue;
  CheckStrMinSize('doc_number', AValue);
  CheckStrMaxSize('doc_number', AValue);
  ModifiedProperty('doc_number');
end;

procedure Tretail_sale.Setdoc_date(AValue: Tdate_type);
begin
  Fdoc_date:=AValue;
  CheckStrMinSize('doc_date', AValue);
  CheckStrMaxSize('doc_date', AValue);
  ModifiedProperty('doc_date');
end;

procedure Tretail_sale.Setaction_id(AValue: Longint);
begin
  Faction_id:=AValue;
  CheckFixedValue('action_id', AValue);
  ModifiedProperty('action_id');
end;

procedure Tretail_sale.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('TRADE_PARTICIPANT_INN', 'TRADE_PARTICIPANT_INN', [xsaSimpleObject], '', 10, 12);
  P:=RegisterProperty('action', 'action', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
    P.ValidList.Add('3');
    P.ValidList.Add('4');
    P.ValidList.Add('5');
    P.ValidList.Add('6');
    P.ValidList.Add('7');
    P.ValidList.Add('8');
    P.ValidList.Add('9');
    P.ValidList.Add('10');
    P.ValidList.Add('11');
    P.ValidList.Add('12');
  P:=RegisterProperty('action_date', 'action_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('doc_type', 'doc_type', [xsaSimpleObject], '', -1, -1);
    P.ValidList.Add('1');
    P.ValidList.Add('2');
    P.ValidList.Add('3');
    P.ValidList.Add('4');
    P.ValidList.Add('5');
    P.ValidList.Add('6');
    P.ValidList.Add('7');
  P:=RegisterProperty('doc_name', 'doc_name', [xsaSimpleObject], '', 1, 200);
  P:=RegisterProperty('doc_number', 'doc_number', [xsaSimpleObject], '', 1, 200);
  P:=RegisterProperty('doc_date', 'doc_date', [xsaSimpleObject], '', 10, 10);
  P:=RegisterProperty('conclusion', 'conclusion', [], '', -1, -1);
  P:=RegisterProperty('action_id', 'action_id', [xsaRequared], '', -1, -1);
    P.DefaultValue:='15';
end;

procedure Tretail_sale.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fconclusion:=Tretail_sale_conclusion.Create;
end;

destructor Tretail_sale.Destroy;
begin
  Fconclusion.Free;
  inherited Destroy;
end;

constructor Tretail_sale.Create;
begin
  inherited Create;
  action_id:=15;
end;

  {  Tretail_sale_conclusion  }
procedure Tretail_sale_conclusion.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('detail', 'detail', [], '', -1, -1);
end;

procedure Tretail_sale_conclusion.InternalInitChilds;
begin
  inherited InternalInitChilds;
  Fdetail:=Tretail_sale_conclusion_detailList.Create;
end;

destructor Tretail_sale_conclusion.Destroy;
begin
  Fdetail.Free;
  inherited Destroy;
end;

constructor Tretail_sale_conclusion.Create;
begin
  inherited Create;
end;

  {  Tretail_sale_conclusion_detail  }
procedure Tretail_sale_conclusion_detail.Setsign_num(AValue: Tgs1_uit_type);
begin
  Fsign_num:=AValue;
  CheckStrMinSize('sign_num', AValue);
  CheckStrMaxSize('sign_num', AValue);
  ModifiedProperty('sign_num');
end;

procedure Tretail_sale_conclusion_detail.Setcost(AValue: Tprice_type);
begin
  Fcost:=AValue;
  CheckMinInclusiveValue('cost', AValue);
  ModifiedProperty('cost');
end;

procedure Tretail_sale_conclusion_detail.Setvat_value(AValue: Tprice_type);
begin
  Fvat_value:=AValue;
  CheckMinInclusiveValue('vat_value', AValue);
  ModifiedProperty('vat_value');
end;

procedure Tretail_sale_conclusion_detail.InternalRegisterPropertys;
var
  P: TPropertyDef;
begin
  inherited InternalRegisterPropertys;
  P:=RegisterProperty('sign_num', 'sign_num', [xsaSimpleObject], '', 31, 44);
  P:=RegisterProperty('cost', 'cost', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
  P:=RegisterProperty('vat_value', 'vat_value', [xsaSimpleObject], '', -1, -1);
    P.TotalDigits := 19;
    P.FractionDigits := 2;
    P.minInclusiveFloat:=0;
end;

procedure Tretail_sale_conclusion_detail.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor Tretail_sale_conclusion_detail.Destroy;
begin
  inherited Destroy;
end;

constructor Tretail_sale_conclusion_detail.Create;
begin
  inherited Create;
end;

end.