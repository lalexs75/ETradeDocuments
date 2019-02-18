{ interface library for FPC and Lazarus

  Copyright (C) 2019 Lagunov Aleksey alexs75@yandex.ru

  Генерация xml файлов для электронного документооборота

  Структуры данных базируются на основании "Приказ ФНС РФ от 19.12.2018 N ММВ-7-15/820@"

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

unit TreasuryInformation;

{$mode objfpc}{$H+}

interface

uses
  Classes, SysUtils, xml_doc;

type

  { TTreasuryInformation }

  TTreasuryInformation = class(TXmlSerializationObject)   //%Таблица 7.16
  private
    FPurchaseID: string;
    procedure SetPurchaseID(AValue: string);
  protected
    procedure InternalRegisterPropertys; override;
    procedure InternalInitChilds; override;
  public
    destructor Destroy; override;
  published
    property PurchaseID:string read FPurchaseID write SetPurchaseID;
    property AccountNumber:string;
    property FinancialName:string;
    property ReestrNumber:string;
    property BudgetAccountNumebr:string;
    property TreasuryCode:string;
    property TreasuryName:string;
    property OKTMOCode:string;
    property OKTMOCodeLocation:string;
    property MaxPayDate:string;
    (*
    Предельная дата оплаты 	ДатаОплПред 	А 	T(=10) 	Н 	Типовой элемент <ДатаТип>.
    Дата в формате ДД.ММ.ГГГГ

    Учетный номер денежного обязательства 	УчНомДенОбяз 	А 	T(=22) 	Н
    Обязателен для заполнения при внесении изменений в ранее направленный в Федеральное казначейство документ, по которому было поставлено на учет денежное обязательство

    Очередность платежа 	ОчерПлат 	А 	T(=1) 	Н
    Обязателен при заполнении ДатаОплПред

    Вид платежа 	ВидПлат 	А 	T(=1) 	НК 	Принимает значение:
    0 - пусто;
    4 - срочно.
    Обязателен при заполнении ДатаОплПред

    Информация для сведений о денежном обязательстве 	ИнфСведДенОбяз 	С 		ОМ
    Состав элемента представлен в таблице 7.17
    *)
  end;

implementation

{ TTreasuryInformation }

procedure TTreasuryInformation.SetPurchaseID(AValue: string);
begin
  if FPurchaseID=AValue then Exit;
  FPurchaseID:=AValue;
end;

procedure TTreasuryInformation.InternalRegisterPropertys;
begin
  (*
  Идентификационный код закупки 	ИдКодЗак 	А 	T(1-36) 	Н

  Номер лицевого счета покупателя 	ЛицСчетПок 	А 	T(=11) 	О

  Наименование финансового органа покупателя 	НаимФинОргПок 	А 	T(1-2000) 	О
  Принимает значение "Министерство финансов Российской Федерации", если покупатель является участником бюджетного процесса федерального уровня. Указывается наименование финансового органа соответствующего бюджета, если покупатель является участником бюджетного процесса субъекта Российской Федерации или муниципального образования

  Номер реестровой записи покупателя по Реестру участников бюджетного процесса, а также юридических лиц, не являющихся участниками бюджетного процесса
  НомРеестрЗапПок 	А 	T(=8) 	О

  Учетный номер бюджетного обязательства покупателя 	УчНомБюдОбязПок 	А 	T(16-19) 	Н

  Код территориального органа Федерального казначейства покупателя 	КодКазначПок 	А 	T(=4) 	Н
  Код территориального органа Федерального казначейства

  Наименование территориального органа Федерального казначейства покупателя 	НаимКазначПок 	А 	T(1-2000) 	Н
  Полное или сокращенное наименование территориального органа Федерального казначейства, в котором открыт лицевой счет покупателя

  Код покупателя в Общероссийском классификаторе территорий муниципальных образований 	ОКТМОПок 	А 	T(8-11) 	ОК
  Типовой элемент <ОКТМОТип>.
  Принимает значение в соответствии с Общероссийским классификатором территорий муниципальных образований

  Код места поставки в Общероссийском классификаторе территорий муниципальных образований 	ОКТМОМесПост 	А 	T(8-11) 	НК
  Типовой элемент <ОКТМОТип>.
  Принимает значение в соответствии с Общероссийским классификатором территорий муниципальных образований

  Предельная дата оплаты 	ДатаОплПред 	А 	T(=10) 	Н 	Типовой элемент <ДатаТип>.
  Дата в формате ДД.ММ.ГГГГ

  Учетный номер денежного обязательства 	УчНомДенОбяз 	А 	T(=22) 	Н
  Обязателен для заполнения при внесении изменений в ранее направленный в Федеральное казначейство документ, по которому было поставлено на учет денежное обязательство

  Очередность платежа 	ОчерПлат 	А 	T(=1) 	Н
  Обязателен при заполнении ДатаОплПред

  Вид платежа 	ВидПлат 	А 	T(=1) 	НК 	Принимает значение:
  0 - пусто;
  4 - срочно.
  Обязателен при заполнении ДатаОплПред

  Информация для сведений о денежном обязательстве 	ИнфСведДенОбяз 	С 		ОМ
  Состав элемента представлен в таблице 7.17
  *)
end;

procedure TTreasuryInformation.InternalInitChilds;
begin
  inherited InternalInitChilds;
end;

destructor TTreasuryInformation.Destroy;
begin
  inherited Destroy;
end;

end.

