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

local RuleConstructor, Rule = {}, {}

local Types = loadstring(game.HttpService:GetAsync("https://github.com/cybe42/exp-lvm/raw/refs/heads/main/Types.lua"))()
local SLVMEnumModule = loadstring(game.HttpService:GetAsync("https://github.com/cybe42/exp-lvm/raw/refs/heads/main/Enum.lua"))()
local SLVMEnum, Utilities = SLVMEnumModule.EnumList, SLVMEnumModule.Utilities

function RuleConstructor.new(Kind: Types.SLVMEnumItem, Data: any?, Message: string?): Types.Rule
	local IsEnumItem = Utilities:IsA(Kind, "EnumItem")
	assert(IsEnumItem or type(Kind) == "string" or type(Kind) == "number", "Expected SLVMEnumItem or string or number as argument #1 for 'Rule.new'")
	assert(type(Message) == "string" or type(Message) == "nil", "Expected string or nil as argument #2 for 'Rule.new'")
	
	--// AutoResolver
	local EnumKind: Types.SLVMEnumItem = Kind;

	if not IsEnumItem then
		if type(Kind) == "number" then
			EnumKind = Utilities:FindEnumItemByValue(SLVMEnum.RuleType, Kind)

			if not EnumKind then
				error("Provided EnumItem value does not exist for the expected SubEnum", 2)
			end
		elseif type(Kind) == "string" then
			EnumKind = Utilities:FindEnumItemByName(SLVMEnum.RuleType, Kind)

			if not EnumKind then
				error("Provided EnumItem value does not exist for the expected SubEnum", 2)
			end
		end
	end

	--// OOP	
	return table.freeze(setmetatable({
		Kind = EnumKind,
		Data = Data,
		Message = Message,
		
		ExtraData = {			
			VariableTypeBlock = nil,
		}
	}, {
		__index = Rule
	}))
end

function Rule:IsA(RuleType: Types.SLVMEnumItem): boolean
	return self.Kind == RuleType
end

return RuleConstructor
