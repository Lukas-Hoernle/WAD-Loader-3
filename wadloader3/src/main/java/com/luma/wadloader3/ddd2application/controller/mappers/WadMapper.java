package com.luma.wadloader3.ddd2application.controller.mappers;

import com.luma.wadloader3.ddd3domain.wad.model.Wad;
import com.luma.wadloader3api.model.WadDto;
import lombok.RequiredArgsConstructor;
import org.springframework.stereotype.Service;

@Service
@RequiredArgsConstructor
public class WadMapper {

    public WadDto map(Wad wad) {
        return WadDto.builder()
                .id(wad.getId())
                .name(wad.getName())
                .description(wad.getDescription())
                .build();
    }
}
