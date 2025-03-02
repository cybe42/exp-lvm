--[[
	This file is a part of the SLVM module.

	SLVM - SecureLuaVirtualMachine
	Copyright (C) 2021 ASense

	This program is free software: you can redistribute it and/or modify
	it under the terms of the GNU General Public License as published by
	the Free Software Foundation, either version 3 of the License, or
	(at your option) any later version.

	This program is distributed in the hope that it will be useful,
	but WITHOUT ANY WARRANTY; without even the implied warranty of
	MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
	GNU General Public License for more details.

	You should have received a copy of the GNU General Public License
	along with this program. If not, see <http://www.gnu.org/licenses/>.
	
	SLVM | By the ASense Team
]]

--// Enum Related
export type SLVMEnumItem = {
	Name: string,
	Value: number,
	EnumType: SLVMSubEnum
}

export type SLVMSubEnum = {
	[string]: SLVMEnumItem
}

export type SLVMEnum = {
	[string]: SLVMSubEnum
}

--// Lazy Types
export type Closure = (...any?) -> ...any?

--// Sandbox Related
export type Rule = {
	Kind: SLVMEnumItem,
	Data: any,
	Message: string?,
	IsA: (self: Rule, RuleType: SLVMEnumItem) -> boolean,

	ExtraData: {
		VariableTypeBlock: SLVMEnumItem,
	}
}

export type SecureLuaVirtualMachine = {
	Rules: {[number]: Rule?},
	ReplacedClosures: {[Closure]: Closure},
	ScriptGlobalType: SLVMEnumItem,
	MaxThreadCount: number,
	
	IsAdditionalSettingEnabled: (self: SecureLuaVirtualMachine, AdditionalSetting: SLVMEnumItem) -> boolean,
	EnableAdditionalSetting: (self: SecureLuaVirtualMachine, AdditionalSetting: SLVMEnumItem) -> nil,
	DisableAdditionalSetting: (self: SecureLuaVirtualMachine, AdditionalSetting: SLVMEnumItem) -> nil,

	AddRule: (self: SecureLuaVirtualMachine, Rule: Rule) -> nil,
	RemoveRule: (self: SecureLuaVirtualMachine, Rule: Rule | number) -> nil,

	Run: (self: SecureLuaVirtualMachine, Code: string, ...any?) -> ...any?,
	Compile: (self: SecureLuaVirtualMachine, Code: string) -> (Closure?, string?),
	
	ReplaceClosure: (self: SecureLuaVirtualMachine, Closure: (...any) -> ...any?, NewClosure: (...any) -> ...any?) -> (...any) -> ...any?,
	AddReadHook: (self: SecureLuaVirtualMachine, Object: {any?} | Instance, Hook: Closure) -> nil,
	AddWriteHook: (self: SecureLuaVirtualMachine, Object: {any?} | Instance, Hook: Closure) -> nil,
	
	CheckCString: (self: SecureLuaVirtualMachine, String: string, CString: string) -> boolean,
	
	ChangeChunkName: (self: SecureLuaVirtualMachine, NewChunkName: string) -> nil,
	DeleteCreatedInstances: (self: SecureLuaVirtualMachine, (Instance, ...any?) -> boolean?) -> nil,
	CloseCreatedThreads: (self: SecureLuaVirtualMachine, (thread, ...any?) -> boolean?) -> nil
}

return nil
