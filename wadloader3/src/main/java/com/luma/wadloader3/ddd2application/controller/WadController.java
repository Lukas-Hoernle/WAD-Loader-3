package com.luma.wadloader3.ddd2application.controller;

import com.luma.wadloader3.ddd2application.controller.mappers.WadMapper;
import com.luma.wadloader3.ddd3domain.files.model.FilePath;
import com.luma.wadloader3.ddd3domain.files.services.WadFileManager;
import com.luma.wadloader3.ddd3domain.wad.model.Wad;
import com.luma.wadloader3.ddd3domain.wad.repos.WadRepo;
import com.luma.wadloader3.ddd4abstraction.functional.Failable;
import com.luma.wadloader3api.api.WadApi;
import com.luma.wadloader3api.model.WadDto;
import jakarta.validation.Valid;
import lombok.RequiredArgsConstructor;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.RestController;
import org.springframework.web.multipart.MultipartFile;

import java.util.List;
import java.util.Optional;

@RestController
@RequiredArgsConstructor
public class WadController implements WadApi {

    private final WadRepo wadRepo;
    private final WadFileManager fileManager;
    private final WadMapper wadMapper;

    @Override
    public ResponseEntity<WadDto> getWad(Integer id) {
        Optional<Wad> wad = wadRepo.findById(id);
        return wad.map(wadMapper::map)
                .map(ResponseEntity::ok)
                .orElseGet(ResponseEntity.badRequest()::build);
    }

    @Override
    public ResponseEntity<List<WadDto>> getWads() {
        List<WadDto> wads = wadRepo.findAll().stream().map(wadMapper::map).toList();
        return ResponseEntity.ok(wads);
    }

    @Override
    public ResponseEntity<WadDto> postWad(@Valid String name, @Valid String description, MultipartFile file) {
        if (wadRepo.existsByName(name)) return ResponseEntity.badRequest().build();

        return switch (Failable.run(file::getInputStream).apply((inputStream) -> fileManager.saveFile(name, inputStream))) {
            case Failable.Success(FilePath filePath) -> {
                Wad wad = Wad.builder()
                        .description(description)
                        .name(name)
                        .filePath(filePath)
                        .build();

                wad = wadRepo.save(wad);

                yield ResponseEntity.ok(wadMapper.map(wad));
            }
            case Failable.Failure(List<String> error) -> {
                System.out.println(error);
                yield ResponseEntity.badRequest().build();
            }
        };
    }
}
