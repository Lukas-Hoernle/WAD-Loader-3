package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3.ddd3domain.services.WadPackService;
import com.luma.wadloader3.ddd3domain.services.WadService;
import com.luma.wadloader3api.api.ApiApi;
import com.luma.wadloader3api.model.NewWadPackDto;
import com.luma.wadloader3api.model.WadDto;
import com.luma.wadloader3api.model.WadPackDto;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

@RestController
@RequiredArgsConstructor
public class ApiController implements ApiApi {

    private final WadPackService wadPackService;
    private final WadService wadService;

    @Override
    public ResponseEntity<WadDto> apiWadIdGet(Integer id) {
        return null;
    }

    @Override
    public ResponseEntity<WadDto> apiWadPost(@Valid String name, @Valid String description, MultipartFile file) {
        return null;
    }

    @Override
    public ResponseEntity<Void> apiWadpackIdDelete(Integer id) {
        return null;
    }

    @Override
    public ResponseEntity<WadPackDto> apiWadpackIdGet(Integer id) {
        return null;
    }

    @Override
    public ResponseEntity<WadPackDto> apiWadpackIdPut(Integer id, @Valid NewWadPackDto newWadPackDto) {
        return null;
    }

    @Override
    public ResponseEntity<WadPackDto> apiWadpackPost(@Valid NewWadPackDto newWadPackDto) {
        return null;
    }
}
