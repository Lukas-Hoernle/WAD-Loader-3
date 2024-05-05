package com.luma.wadloader3.ddd2application.controller.mappers;

import com.luma.wadloader3.ddd3domain.wad.model.WadPack;
import com.luma.wadloader3api.model.WadDto;
import com.luma.wadloader3api.model.WadPackDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

import java.util.List;
import java.util.Map;

@Service
@RequiredArgsConstructor
public class WadPackMapper {
    private final WadMapper wadMapper;

    public WadPackDto map(WadPack wadPack) {

        List<WadDto> wads = wadPack.getWads()
                .entrySet()
                .stream()
                .sorted(Map.Entry.comparingByKey())
                .map(Map.Entry::getValue)
                .map(wadMapper::map)
                .toList();

        return WadPackDto.builder()
                .description(wadPack.getDescription())
                .name(wadPack.getName())
                .id(wadPack.getId())
                .wads(wads)
                .build();
    }
}
