package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3api.api.WadApi;
import com.luma.wadloader3api.model.WadDto;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

@RestController
@RequiredArgsConstructor
public class WadController implements WadApi {
    @Override
    public ResponseEntity<WadDto> wadIdGet(Integer id) {
        return null;
    }

    @Override
    public ResponseEntity<WadDto> wadPost(@Valid String name, @Valid String description, MultipartFile file) {
        return null;
    }
}
