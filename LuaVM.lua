--[[
	This file is a part of the SLVM module.
	The file structure follows the same as vLua:
		- https://create.roblox.com/marketplace/asset/4689019964/vLua-Loadstring-reimplemented-in-Lua

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

local Compile = loadstring(game.HttpService:GetAsync("https://github.com/cybe42/exp-lvm/raw/refs/heads/main/Yueliang.lua"))()
local Wrap = loadstring(game.HttpService:GetAsync("https://github.com/cybe42/exp-lvm/raw/refs/heads/main/FiOne.lua"))()

return function(source, chunkName, env, slvmrules)
	local compiled, wrapped = pcall(function()
		local compiledBytecode = Compile(source, chunkName or env.script and env.script:GetFullName())
		return Wrap(compiledBytecode, slvmrules, env)
	end)
	
	return compiled and wrapped, (not compiled) and wrapped
end
