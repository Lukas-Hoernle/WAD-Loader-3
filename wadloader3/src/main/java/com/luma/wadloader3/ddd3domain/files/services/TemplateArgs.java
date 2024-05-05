package com.luma.wadloader3.ddd3domain.files.services;

import com.luma.wadloader3.ddd3domain.wad.model.Wad;

import java.util.Map;

public record TemplateArgs(Map<Integer, Wad> wads, String name) {
}
