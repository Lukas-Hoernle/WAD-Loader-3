package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3api.api.WadpackApi;
import com.luma.wadloader3api.model.NewWadPackDto;
import com.luma.wadloader3api.model.WadPackDto;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequiredArgsConstructor
public class WadPackController implements WadpackApi {
    @Override
    public ResponseEntity<Void> wadpackIdDelete(Integer id) {
        return null;
    }

    @Override
    public ResponseEntity<WadPackDto> wadpackIdGet(Integer id) {
        return null;
    }

    @Override
    public ResponseEntity<WadPackDto> wadpackIdPut(Integer id, @Valid NewWadPackDto newWadPackDto) {
        return null;
    }

    @Override
    public ResponseEntity<WadPackDto> wadpackPost(@Valid NewWadPackDto newWadPackDto) {
        return null;
    }
}
